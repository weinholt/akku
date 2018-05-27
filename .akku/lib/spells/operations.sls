#!r6rs #!r6rs
;;; operations.sls --- T-like operations.

;; Copyright (C) 2009, 2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;@ A generic dispatch system similiar to operations in T.
(library (spells operations)
  (export object
          operation
          define-operation
          join)
  (import (rnrs base)
          (rnrs lists)
          (only (srfi :1 lists) any)
          (spells procedure-annotations))
  
;; Auxiliary syntax
(define-syntax %method-clauses->handler
  (syntax-rules ()
    ((%method-clauses->handler ((?op . ?params) ?body ...) ...)
     (let ((methods (list (cons ?op (lambda ?params ?body ...)) ...)))
       (lambda (op)
         (cond ((assq op methods) => cdr)
               (else #f)))))))

;;@defspec object procedure method-clause ...
;;
;; Create an object with default handler @var{procedure} and methods
;; as specified by @var{method-clause} @dots{}.  Each
;; @var{method-clause} must be of the form @code{((@var{op}
;; . @var{parameters}) @var{body} ...)}, where @var{op} is an
;; operation as obtained by @code{operation} or
;; @code{define-operation}.
;;
;;@end defspec
(define-syntax object
  (syntax-rules ()
    ((object ?proc ?method-clause ...)
     (make-object ?proc (%method-clauses->handler ?method-clause ...)))))

;;@stop

(define (make-object proc handler)
  (annotate-procedure
   (or proc (lambda args (error 'make-object "object is not applicable"))) handler))

;;@defspec operation default method-clause ...
;;
;; Create an operation with default handler @var{default} and methods
;; as specified by @var{method-clause} @dots{}. The method clauses
;; take the same form as with @code{operation}.
;;
;;@end defspec
(define-syntax operation
  (syntax-rules ()
    ((operation "%named" ?name ?default ?method-clause ...)
     (make-operation '?name ?default (%method-clauses->handler ?method-clause ...)))
    ((operation ?default ?method-clause ...)
     (operation "%named" #f ?default ?method-clause ...))))

;;@stop

(define (make-operation name default handler)
  (letrec ((op (make-object
                (lambda (obj . args)
                  (cond ((and (procedure? obj) ((procedure-annotation obj) op))
                         => (lambda (method)
                              (apply method obj args)))
                        (default
                          (apply default obj args))
                        (else
                         (error 'operation
                                "operation is not available"
                                obj
                                (or name op)))))
                handler)))
    op))

;;@defspec define-operation (name . parameters) body ...
;;@defspecx define-operation (name . parameters)
;;
;; Define @var{name} as an operation with a default handler with the
;; argument list @var{parameters} and the body @var{body}.  If there
;; is no @var{body}, the operation will not have a default handler.
;;
;;@end defspec
(define-syntax define-operation
  (syntax-rules ()
    ((define-operation (?name . ?args))
     (define ?name (operation "%named" ?name #f)))
    ((define-operation (?name . ?args) ?body1 ?body ...)
     (define ?name (operation "%named" ?name (lambda ?args ?body1 ?body ...))))))

;;@ Create a compound object from @var{object1} and @var{objects}.
;; The returned object will respond to any operation defined on
;; @var{object1} or @var{objects}; operations are resolved in the
;; order of @code{join}'s arguments. If there is no matching operation
;; found, @var{object1}'s default handler will be invoked, if any.
(define (join object1 . objects)
  (make-object object1
               (lambda (op)
                 (let ((method (any (lambda (o) ((procedure-annotation o) op))
                                    (cons object1 objects))))
                   (or method
                       (error 'join "operation not available" objects op))))))

)
