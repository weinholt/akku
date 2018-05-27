#!r6rs #!r6rs
;;; alist.sls --- R6RS library providing alist utilities.

;; Copyright (C) 2009-2011, 2015 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

;;@ Association list utilities.
(library (spells alist)
  (export acons
          assq-ref assv-ref assoc-ref
          let-assq)
  (import (for (rnrs base) run expand)
          (for (rnrs syntax-case) expand)
          (for (rnrs lists) run expand)
          (for (srfi :8 receive) expand))

;;@ Return the alist @3 extended by @code{(cons @1 @2)}.
(define (acons key val alist)
  (cons (cons key val) alist))

;;@ Return the @code{cdr} of the entry in the alist @1 referred to by
;; @2 or @code{#f} if no such entry exists.
(define (assq-ref alist key)
  (cond ((assq key alist) => cdr) (else #f)))
(define (assv-ref alist key)
  (cond ((assv key alist) => cdr) (else #f)))
(define (assoc-ref alist key)
  (cond ((assoc key alist) => cdr) (else #f)))

;;@stop

(define-syntax let-assq
  (lambda (stx)
    (define (valid-binding? b)
      (syntax-case b ()
        ((id key) (and (identifier? #'id) (identifier? #'key)) #t)
        (id (identifier? #'id) #t)
        (_ #f)))
    (syntax-case stx ()
      ((_ alist-expr (binding ...) body ...)
       (for-all valid-binding? #'(binding ...))
       (with-syntax (((let-binding ...)
                      (map (lambda (b)
                             (receive (id key)
                                      (syntax-case b ()
                                        ((id key)
                                         (values #'id #'key))
                                        (id
                                         (values #'id #'id)))
                               #`(#,id (assq-ref alist '#,key))))
                           #'(binding ...))))
         #'(let ((alist alist-expr))
             (let (let-binding ...)
               body ...)))))))

)
