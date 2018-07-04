#!r6rs ;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2018 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

(library (semver ranges)
  (export
    string->semver-range
    semver-range-desugar
    semver-range->string
    semver-range->matcher
    semver-in-range?)
  (import
    (rnrs)
    (srfi :67 compare-procedures)
    (packrat)
    (semver versions)
    (xitomatl AS-match))

(define range-parser
  (packrat-parser
   (begin
     (define (space results)
       (if (eqv? (parse-results-token-value results) #\space)
           (space (parse-results-next results))
           (nothing results)))
     (define (v= results)
       (if (memv (parse-results-token-value results) '(#\v #\=))
           (v= (parse-results-next results))
           (nothing results)))
     (define (pnumber starting-results)
       (let ((ch (parse-results-token-value starting-results)))
         (if (eqv? ch #\0)
             (make-expected-result (parse-results-position starting-results) ch)
             (let loop ((acc '()) (results starting-results))
               (let ((ch (parse-results-token-value results)))
                 (if (and (char? ch) (char<=? #\0 ch #\9))
                     (loop (cons ch acc) (parse-results-next results))
                     (let ((n (string->number (list->string (reverse acc)))))
                       (if n
                           (make-result n results)
                           (make-expected-result (parse-results-position starting-results)
                                                 'id)))))))))
     (define (id pre/build)
       (lambda (starting-results)
         (let loop ((all-digits #t) (acc '()) (results starting-results))
           (let ((ch (parse-results-token-value results)))
             (cond
               ((and (char? ch)
                     (or (char<=? #\0 ch #\9) (char-ci<=? #\A ch #\Z) (char=? ch #\-)))
                (loop (and all-digits (char<=? #\0 ch #\9))
                      (cons ch acc)
                      (parse-results-next results)))
               ((null? acc)
                (make-expected-result (parse-results-position starting-results)
                                      'id))
               ((and (eq? pre/build 'pre) all-digits)
                (let* ((s (list->string (reverse acc)))
                       (n (string->number s 10)))
                  (if (and (> (string-length s) 1) (char=? #\0 (string-ref s 0)))
                      (make-expected-result (parse-results-position starting-results)
                                            'id)
                      (make-result n results))))
               (else
                (let ((s (list->string (reverse acc))))
                  (make-result s results))))))))
     range-set)
   (range-set  ((a <- range logical-or b <- range-set)                `(or ,a ,b))
               ((a <- range '#f)                                      a))
   (logical-or ((space '#\| '#\| space)                               'or))
   (nothing    (()                                                    'nothing))
   (range      ((a <- hyphen)                                         a)
               ((a <- simple '#\space b <- simple)                    `(and ,a ,b))
               ((a <- simple)                                         a)
               (()                                                    `(= #(* #f #f () ()))))
   (hyphen     ((a <- partial '#\space '#\- '#\space b <- partial)    `(- ,a ,b)))
   (simple     ((a <- primitive)                                      a)
               ((a <- partial)                                        a)
               ((a <- tilde)                                          a)
               ((a <- caret)                                          a))
   (primitive  (('#\< '#\= a <- partial)                              `(<= ,a))
               (('#\> '#\= a <- partial)                              `(>= ,a))
               (('#\< a <- partial)                                   `(< ,a))
               (('#\> a <- partial)                                   `(> ,a))
               (('#\= a <- partial)                                   `(= ,a))
               ((a <- partial)                                        `(= ,a)))
   (partial    ((v= a <- nr '#\. b <- xr '#\. c <- xr q <- qualifier) `#(,a ,b ,c ,@q))
               ((v= a <- xr '#\. b <- xr)                             `#(,a ,b #f () ()))
               ((v= a <- xr)                                          `#(,a #f #f () ())))
   (xr         (('#\x)                                                '*)
               (('#\X)                                                '*)
               (('#\*)                                                '*)
               ((a <- nr)                                             a))
   (nr         (('#\0)                                                0)
               ((n <- pnumber)                                        n))
   (tilde      (('#\~ a <- partial)                                   `(~ ,a)))
   (caret      (('#\^ a <- partial)                                   `(^ ,a)))
   (qualifier  (('#\- a <- pre '#\+ b <- build)                       `(,a ,b))
               (('#\- a <- pre)                                       `(,a ()))
               (('#\+ a <- build)                                     `(() ,a))
               (()                                                    `(() ())))
   (pre        ((a <- pre-ids)                                        a))
   (build      ((a <- build-ids)                                      a))
   (pre-ids    ((a <- (id 'pre) '#\. b <- pre-ids)                    `(,a . ,b))
               ((a <- (id 'pre))                                      `(,a)))
   (build-ids  ((a <- (id 'build) '#\. b <- build-ids)                `(,a . ,b))
               ((a <- (id 'build))                                    `(,a)))))

(define (string->semver-range x)
  (let ((g (packrat-string-results #f x)))
    (let ((result (range-parser g)))
      (if (parse-result-successful? result)
          (parse-result-semantic-value result)
          (let ((e (parse-result-error result)))
            (assertion-violation 'string->semver-range
                                 "Parse error" x
                                 (parse-position-column (parse-error-position e))
                                 (parse-error-expected e)
                                 (parse-error-messages e)))))))

(define (op? x)
  (memq x '(< > <= >= =)))

(define semver-range-desugar
  (match-lambda
   [('or a b) `(or ,(semver-range-desugar a) ,(semver-range-desugar b))]
   [('and a b) `(and ,(semver-range-desugar a) ,(semver-range-desugar b))]

   ;; Hyphen ranges. a.b.c - x.y.z.
   [('- #(x0 y0 z0 p0 b0) #(x1 y1 z1 p1 b1))
    `(and (>= #(,x0 ,(or y0 0) ,(or z0 0) ,p0 ,b0))
          ,(cond
             ((not y1) `(< #(,(+ x1 1) ,0 0 ,p1 ,b1)))
             ((not z1) `(< #(,x1 ,(+ y1 1) 0 ,p1 ,b1)))
             (else     `(<= #(,x1 ,y1 ,z1 ,p1 ,b1)))))]

   ;; X-ranges
   [((or '<= '= '>=) #('* _ _ _ _))     ;*
    '(>= #(0 0 0 () ()))]
   [('= #(x (or #f '*) _ _ _))          ;=x =x.*
    `(and (>= #(,x 0 0 () ()))
          (< #(,(+ x 1) 0 0 () ())))]
   [('= #(x y (or #f '*) _ _))          ;=x.y =x.y.*
    `(and (>= #(,x ,y 0 () ()))
          (< #(,x ,(+ y 1) 0 () ())))]

   ;; Tilde ranges
   [('~ #(x (or #f '*) _ () _))         ;~x ~x.*
    `(and (>= #(,x 0 0 () ()))
          (< #(,(+ x 1) 0 0 () ())))]
   [('~ #(x y (or #f '*) () _))         ;~x.y ~x.y.*
    `(and (>= #(,x ,y 0 () ()))
          (< #(,x ,(+ y 1) 0 () ())))]
   [('~ #(x y z p b))                   ;~x.y.z
    `(and (>= #(,x ,y ,z ,p ,b))
          (< #(,x ,(+ y 1) 0 () ())))]

   ;; Caret ranges
   [('^ #(x (or #f '*) _ _ _))          ;^x ^x.*
    `(and (>= #(,x 0 0 () ()))
          (< #(,(+ x 1) 0 0 () ())))]
   [('^ #(0 y (or #f '*) _ _))          ;^0.y 0.y.*
    `(and (>= #(0 ,y 0 () ()))
          (< #(0 ,(+ y 1) 0 () ())))]
   [('^ #(x y (or #f '*) _ _))          ;^x.y ^x.y.*
    `(and (>= #(,x ,y 0 () ()))
          (< #(,(+ x 1) 0 0 () ())))]
   [('^ #(0 0 z p _))                   ;^0.0.z
    `(and (>= #(0 0 ,z ,p ()))
          (< #(0 0 ,(+ z 1) () ())))]
   [('^ #(0 y z p _))                   ;^0.y.z
    `(and (>= #(0 ,y ,z ,p ()))
          (< #(0 ,(+ y 1) 0 () ())))]
   [('^ #(x y z p _))                   ;^x.y.z
    `(and (>= #(,x ,y ,z ,p ()))
          (< #(,(+ x 1) 0 0 () ())))]

   ;; For comparators * and missing desugars to zero.
   [((? op? op) #((or #f '*) _ _ _ _))  ;>=x >=*
    `(,op #(0 0 0 () ()))]
   [((? op? op) #(x (or #f '*) _ _ _))
    `(,op #(,x 0 0 () ()))]
   [((? op? op) #(x y (or #f '*) _ _))
    `(,op #(,x ,y 0 () ()))]

   ;; Fallthrough
   [((? op? op) #(x y z p b))
    `(,op #(,x ,y ,z ,p ,b))]

   (x (error 'semver-range-desugar "Invalid range" x))))

(define (semver-range->string range)
  (define (put-id-list p ids)
    (display (car ids) p)
    (unless (null? (cdr ids))
      (put-char p #\.)
      (put-id-list p (cdr ids))))
  (define (put-version p v)
    (match v
      [#(major minor patch pre build)
       (put-datum p major)
       (when minor
         (put-char p #\.)
         (put-datum p minor)
         (when patch
           (put-char p #\.)
           (put-datum p patch)
           (when (pair? pre)
             (put-char p #\-)
             (put-id-list p pre))
           (when (pair? build)
             (put-char p #\+)
             (put-id-list p build))))]))
  (call-with-string-output-port
    (lambda (p)
      (let lp ((x range))
        (match x
          [('= (and #('* #f #f () ()) v))
           (put-version p v)]
          [((and (or '< '> '<= '>= '= '~ '^) op) v)
           (put-datum p op)
           (put-version p v)]
          [('- v0 v1)
           (put-version p v0) (put-string p " - ") (put-version p v1)]
          [('or e0 e1)
           (lp e0) (put-string p " || ") (lp e1)]
          [('and e0 e1)
           (lp e0) (put-string p " ") (lp e1)]
          [x
           (error 'semver-range->string "Invalid range" x range)])))))

(define (semver-range->matcher range)
  (let* ((range (if (string? range) (string->semver-range range) range))
         (range (semver-range-desugar range))
         (allowed-pre-releases
          (let lp ((range range))
            (match range
              [((or 'or 'and) e0 e1)
               (append (lp e0) (lp e1))]
              [(_op #(x y z p b))
               (if (null? p)
                   '()
                   (list (make-semver x y z)))]
              [x (error 'semver-range->matcher "Invalid range" x range)]))))
    (let ((m (let lp ((range range))
               (match range
                 [('or e0 e1)
                  (let ((m0 (lp e0)) (m1 (lp e1)))
                    (lambda (version) (or (m0 version) (m1 version))))]
                 [('and e0 e1)
                  (let ((m0 (lp e0)) (m1 (lp e1)))
                    (lambda (version) (and (m0 version) (m1 version))))]
                 [(op #(x y z p b))
                  (let ((=? (case op
                              ((<) <?)
                              ((>) >?)
                              ((<=) <=?)
                              ((>=) >=?)
                              ((=) =?)
                              (else (error 'semver-range->matcher
                                           "Invalid op" op range)))))
                    (let ((semver (make-semver x y z p b)))
                      (lambda (version)
                        (and (=? semver-compare version semver)
                             (or (null? (semver-pre-release-ids version))
                                 (exists
                                  (lambda (allowed)
                                    (and (= (semver-major version) (semver-major allowed))
                                         (= (semver-minor version) (semver-minor allowed))
                                         (= (semver-patch version) (semver-patch allowed))))
                                  allowed-pre-releases))))))]
                 [x
                  (error 'semver-range->matcher "Invalid range" x range)]))))
      (lambda (version)
        (m (if (semver? version) version (string->semver version)))))))

(define (semver-in-range? semver range)
  (let ((semver (if (semver? semver) semver (string->semver semver)))
        (range (if (string? range) (string->semver-range range))))
    ((semver-range->matcher range) semver))))
