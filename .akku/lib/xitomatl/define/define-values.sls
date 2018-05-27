#!r6rs #!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl define define-values)
  (export
    define-values)
  (import
    (rnrs)
    (for (only (xitomatl macro-utils)
               formals-ok?/raise with-syntax* gen-temp)
         expand)
    (only (xitomatl common) format))
  
  (define (define-values-error expected received-vals)
    (apply assertion-violation 'define-values
      (format "expected ~a values, received ~a values" expected (length received-vals))
      received-vals))
  
  (define-syntax define-values
    (lambda (stx)
      (define (make-define id index t)
        #`(define #,id (vector-ref #,t #,index)))
      (define (make-last-define id index t)
        #`(define #,id
            (let ((x (vector-ref #,t #,index)))
              (set! #,t #F)
              x)))
      (syntax-case stx ()
        ((_ (id ... . rid) expr)
         (formals-ok?/raise #'(id ... . rid) stx)
         (with-syntax*
             ((t (gen-temp))
              ((def ...)
               (let loop ((frmls #'(id ... . rid))
                          (i 0)
                          (a '()))
                 (syntax-case frmls ()
                   ((x)
                    (reverse (cons (make-last-define #'x i #'t) a)))
                   ((x . r)
                    (loop #'r (+ 1 i) (cons (make-define #'x i #'t) a)))
                   (()
                    '())
                   (x
                    (reverse (cons (make-last-define #'x i #'t) a)))))))
           #`(begin
               (define t
                 (call-with-values
                   (lambda () #F expr)  ;; #F first to prevent internal defines
                   (case-lambda
                     ((id ... . rid)
                      (vector id ... #,@(if (identifier? #'rid) (list #'rid) '())))
                     (otherwise
                      (define-values-error #,(length #'(id ...)) otherwise)))))
               def ...))))))  
  
)
