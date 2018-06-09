;;; Copyright (c) 2006, 2007 Abdulaziz Ghuloum and Kent Dybvig
;;; Copyright © 2017, 2018 Göran Weinholt <goran@weinholt.se>
;;; SPDX-License-Identifier: MIT
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining a
;;; copy of this software and associated documentation files (the "Software"),
;;; to deal in the Software without restriction, including without limitation
;;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;;; and/or sell copies of the Software, and to permit persons to whom the
;;; Software is furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be included in
;;; all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL
;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

;; Library name implementations for various Scheme implementations.

(library (akku lib library-name)
  (export
    library-name->file-name/chezscheme
    library-name->file-name/ikarus
    library-name->file-name/ironscheme
    library-name->file-name/psyntax
    library-name->file-name/racket
    library-name->file-name-variants)
  (import
    (rnrs (6))
    (only (srfi :13 strings) string-index string-prefix?)
    (only (rnrs r5rs) quotient remainder))

;; From the psyntax version in r6rs-libraries.
(define (library-name->file-name/psyntax x)
  (let-values (((p extract) (open-string-output-port)))
    (define (display-hex n)
      (cond
        ((<= 0 n 9) (display n p))
        (else (display
               (integer->char
                (+ (char->integer #\a)
                   (- n 10)))
               p))))
    (let f ((ls x))
      (unless (null? ls)
        (display "/" p)
        (for-each
         (lambda (c)
           (cond
             ((or (char<=? #\a c #\z)
                  (char<=? #\A c #\Z)
                  (char<=? #\0 c #\9)
                  (memv c '(#\- #\. #\_ #\~)))
              (display c p))
             (else
              (display "%" p)
              (let ((n (char->integer c)))
                (when (> n #xff)
                  ;; XXX: Without this check, λ turns into a control
                  ;; character in the filename.
                  (error 'library-name->file-name/psyntax
                         "Characters above U+00FF are not supported" c))
                (display-hex (quotient n 16))
                (display-hex (remainder n 16))))))
         (string->list
          (symbol->string (car ls))))
        (f (cdr ls))))
    (extract)))

;; From Ikarus.
(define (library-name->file-name/ikarus ls)
  (let-values (((p extract) (open-string-output-port)))
    (define (display-hex n)
      (cond
        ((<= 0 n 9) (display n p))
        (else (write-char
               (integer->char
                (+ (char->integer #\a)
                   (- n 10)))
               p))))
    (define (main*? x)
      (and (>= (string-length x) 4)
           (string=? (substring x 0 4) "main")
           (for-all (lambda (x) (char=? x #\_))
                    (string->list (substring x 4 (string-length x))))))
    (let f ((x (car ls)) (ls (cdr ls)) (fst #t))
      (write-char #\/ p)
      (let ([name (symbol->string x)])
        (for-each
         (lambda (n)
           (let ([c (integer->char n)])
             (cond
               ((or (char<=? #\a c #\z)
                    (char<=? #\A c #\Z)
                    (char<=? #\0 c #\9)
                    (memv c '(#\. #\- #\+ #\_)))
                (write-char c p))
               (else
                (write-char #\% p)
                (display-hex (quotient n 16))
                (display-hex (remainder n 16))))))
         (bytevector->u8-list (string->utf8 name)))
        (if (null? ls)
            (when (and (not fst) (main*? name)) (write-char #\_ p))
            (f (car ls) (cdr ls) #f))))
    (extract)))

(define (library-name->file-name/ironscheme x)
  (let-values (((p extract) (open-string-output-port)))
    (define (display-hex n)
      (cond
        ((<= 0 n 9) (display n p))
        (else (display
               (integer->char
                (+ (char->integer #\a) ; lowercase
                   (- n 10)))
               p))))
    (let f ((ls x))
      (unless (null? ls)
        (display "/" p)
        (for-each
         (lambda (c)
           (cond
             ((or (char<=? #\a c #\z)
                  (char<=? #\A c #\Z)
                  (char<=? #\0 c #\9)
                  (memv c '(#\- #\. #\_ #\~)))
              (display c p))
             (else
              (display "%" p)
              (let ((n (char->integer c)))
                (when (> n #xff)
                  ;; XXX: Without this check, λ turns into a control
                  ;; character in the filename.
                  (error 'library-name->file-name/ironscheme
                         "Characters above U+00FF are not supported" c))
                (display-hex (quotient n 16))
                (display-hex (remainder n 16))))))
         (string->list
          (let ((d (symbol->string (car ls))))
            (if (and (char=? #\: (string-ref d 0)) (char<=? #\0 (string-ref d 1) #\9))
                (substring d 1 (string-length d))
                d))))
        (f (cdr ls))))
    (extract)))

;; Chez Scheme, based on observed behavior.
(define (library-name->file-name/chezscheme ls)
  (when (string-prefix? "~" (symbol->string (car ls)))
    (error 'library-name->file-name/chezscheme
           "Refusing to create an absolute path" ls))
  (call-with-string-output-port
    (lambda (p)
      (for-each
       (lambda (component)
         (put-char p #\/)
         (let ((str (symbol->string component)))
           (when (string-index str #\/)
             ;; Chez does not have this check, but an extra / would
             ;; make trouble when the library file is written.
             (error 'library-name->file-name/chezscheme
                    "Refusing to create a path with an extra /" ls))
           (put-string p str)))
       ls))))

;; Racket, based on documented behavior.
(define (library-name->file-name/racket ls)
  ;; https://docs.racket-lang.org/r6rs/libpaths.html
  (let ((name (library-name->file-name/ikarus ls)))
    (if (null? (cdr ls))
        (string-append name "/main")
        name)))

(define (library-name->file-name-variants implementation)
  (case implementation
    ((chezscheme)
     (list library-name->file-name/chezscheme))
    ((ikarus)
     (list library-name->file-name/ikarus))
    ((ironscheme)
     (list library-name->file-name/ironscheme))
    ((mzscheme)
     (list library-name->file-name/racket))
    (else
     ;; If the library is not implementation-dependent, then it
     ;; could be loaded from any one of these filenames.
     (list library-name->file-name/chezscheme
           library-name->file-name/ikarus
           library-name->file-name/ironscheme
           library-name->file-name/psyntax
           library-name->file-name/racket)))))
