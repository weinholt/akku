#!r6rs ;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2018 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

;; This parser is based on Appendix IV in this document:
;;
;;    Software Package Data Exchange (SPDX®) Specification – Version 2.1
;;    https://spdx.org/specifications
;;
;; The given ABNF is unfortunately not very useful.

(library (spdx parser)
  (export
    parse-license-expression
    format-license-expression)
  (import
    (rnrs)
    (packrat))

  (define license-parser
    (packrat-parser
     (begin
       (define (space results)
         (let ((token (parse-results-token-value results)))
           (if (and (char? token) (char-whitespace? token))
               (space (parse-results-next results))
               (nothing results))))
       (define (token str)              ;from json.ss in packrat
         (lambda (starting-results)
           (let loop ((pos 0) (results starting-results))
             (if (= pos (string-length str))
                 (make-result str results)
                 (let ((ch (parse-results-token-value results)))
                   (if (and (char? ch) (char-ci=? ch (string-ref str pos)))
                       (loop (+ pos 1) (parse-results-next results))
                       (make-expected-result (parse-results-position starting-results) str)))))))
       (define (idstring starting-results)
         (let lp ((acc '()) (results starting-results))
           (let ((c (parse-results-token-value results)))
             (cond
               ((and (char? c)
                     (or (char-ci<=? #\A c #\Z) (char<=? #\0 c #\9)
                         (char=? c #\-) (char=? c #\.)))
                (lp (cons c acc)
                    (parse-results-next results)))
               ((null? acc)
                (make-expected-result (parse-results-position starting-results)
                                      'id))
               (else
                (let ((s (list->string (reverse acc))))
                  (make-result s results)))))))
       (define license-id idstring)
       (define exception-id idstring)
       expr)
     (expr         [(a <- or-expr '#f)                                       a])
     (or-expr      [(a <- and-expr space (token "OR") space b <- or-expr)    `(or ,a ,b)]
                   [(a <- and-expr)                                          a])
     (and-expr     [(a <- with-expr space (token "AND") space b <- and-expr) `(and ,a ,b)]
                   [(a <- with-expr)                                         a])
     (with-expr    [(a <- simple-expr space (token "WITH") space b <- exception-id)
                    `(with ,a ,b)]
                   [(a <- simple-expr)                        a])
     (simple-expr  [(a <- ref)                                a]
                   [(a <- license-id '#\+)                    `(+ ,a)]
                   [(a <- license-id)                         a]
                   [('#\( a <- or-expr '#\))                  a])
     (document-ref [((token "DocumentRef-") a <- idstring)    a])
     (license-ref  [((token "LicenseRef-") a <- idstring)     a])
     (ref          [(a <- document-ref '#\: b <- license-ref) `(user-defined ,a ,b)]
                   [(a <- license-ref)                        `(user-defined #f ,a)])
     (nothing      [() #f])))

(define (parse-license-expression x)
  (let ((g (packrat-string-results #f x)))
    (let ((result (license-parser g)))
      (if (parse-result-successful? result)
          (parse-result-semantic-value result)
          (let ((e (parse-result-error result)))
            (assertion-violation 'string->semver-range
                                 "Parse error" x
                                 (parse-position-column (parse-error-position e))
                                 (parse-error-expected e)
                                 (parse-error-messages e)))))))

(define (format-license-expression expr)
  (call-with-string-output-port
    (lambda (p)
      (let lp ((x expr) (paren #t))
        (cond ((string? x)
               (put-string p x))
              ((and (pair? x) (eq? (car x) 'or) (= (length x) 3))
               (when paren (put-char p #\())
               (lp (cadr x) #t)
               (put-string p " OR ")
               (lp (caddr x) (not (and (pair? (caddr x)) (eq? (caaddr x) 'or))))
               (when paren (put-char p #\))))
              ((and (pair? x) (eq? (car x) 'and) (= (length x) 3))
               (when paren (put-char p #\())
               (lp (cadr x) #t)
               (put-string p " AND ")
               (lp (caddr x) (not (and (pair? (caddr x)) (eq? (caaddr x) 'and))))
               (when paren (put-char p #\))))
              ((and (pair? x) (eq? (car x) '+) (= (length x) 2))
               (put-string p (cadr x))
               (put-char p #\+))
              ((and (pair? x) (eq? (car x) 'with) (= (length x) 3))
               (when paren (put-char p #\())
               (lp (cadr x) #t)
               (put-string p " WITH ")
               (put-string p (caddr x))
               (when paren (put-char p #\))))
              ((and (pair? x) (eq? (car x) 'user-defined) (= (length x) 3))
               (when (cadr x)
                 (put-string p "DocumentRef-")
                 (put-string p (cadr x))
                 (put-char p #\:))
               (put-string p "LicenseRef-")
               (put-string p (caddr x)))
              (else
               (assertion-violation 'format-license-expression
                                    "Invalid expression" x expr))))))))
