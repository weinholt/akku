#!r6rs #!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl define)
  (export 
    define-values
    define/who
    define/AV
    define/?
    define/?/AV)
  (import 
    (rnrs)
    (xitomatl define define-values)
    (for (only (xitomatl macro-utils) syntax->list) expand)
    (only (xitomatl common) format)
    (only (xitomatl exceptions) assertion-violation/conditions)
    (xitomatl conditions))
  
  (define-syntax who-wrap
    (lambda (stx)
      (syntax-case stx ()
        ((_ ctxt name expr)
         (with-syntax ((who (datum->syntax #'ctxt 'who)))
           #'(let ((who 'name))
               #F  ;; prevent internal defines in expr 
               expr))))))
  
  (define-syntax define/who
    (lambda (stx)
      (syntax-case stx ()
        ((_ (fname . frmls) b0 b ...)
         (identifier? #'fname)
         #'(define/who fname
             (lambda frmls b0 b ...)))
        ((_ name expr)
         (identifier? #'name)
         #'(define name
             (who-wrap name name 
               expr))))))
  
  
  (define (make-AV who)
    (lambda (msg . irrts) 
      (apply assertion-violation who msg irrts)))
  
  (define-syntax AV-wrap
    (lambda (stx)
      (syntax-case stx ()
        ((_ ctxt name expr)
         (with-syntax ((AV (datum->syntax #'ctxt 'AV)))
           #'(let ((AV (make-AV 'name)))
               #F  ;; prevent internal defines in expr 
               expr))))))
  
  (define-syntax define/AV
    (lambda (stx)
      (syntax-case stx ()
        ((_ (fname . frmls) b0 b ...)
         (identifier? #'fname)
         #'(define/AV fname
             (lambda frmls b0 b ...)))
        ((_ name expr)
         (identifier? #'name)
         #'(define name
             (AV-wrap name name 
               expr))))))
  
  (define (make-arg-check-failed who)
    (lambda (pred-form arg-name arg-value)
      (assertion-violation/conditions who "argument check failed" (list arg-value)
        (make-argument-name-condition arg-name) 
        (make-predicate-expression-condition pred-form))))
  
  (define-syntax case-lambda/?--meta
    (lambda (stx)
      (define (frml-id frml)
        (syntax-case frml () ((id pred) #'id) (_ frml)))
      (define (needs-check? frml)
        (syntax-case frml () ((id pred) #T) (_ #F)))
      (syntax-case stx ()
        ((_ fname (frmls . body) ...)
         (with-syntax ((((f ... fr) ...) 
                        (map (lambda (f)
                               (syntax-case f ()
                                 ((f ... . #(r p)) #'(f ... (r p)))
                                 ((f ... . r) #'(f ... r))))
                             #'(frmls ...))))
           (with-syntax ((((id ... idr) ...)
                          (map (lambda (fl) (map frml-id (syntax->list fl)))
                               #'((f ... fr) ...)))
                         ((((cid p) ...) ...) 
                          (map (lambda (fl) (filter needs-check? (syntax->list fl))) 
                               #'((f ... fr) ...))))
             #'(let ((acf (make-arg-check-failed 'fname)))
                 (case-lambda 
                   ((id ... . idr)
                    (unless (p cid) (acf 'p 'cid cid))
                    ...
                    (let () . body))
                   ...))))))))
  
  (define-syntax define/?
    (lambda (stx)
      (syntax-case stx ()
        ((_ (fname . frmls) body0 body* ...)
         (identifier? #'fname)
         #'(define fname
             (case-lambda/?--meta fname (frmls body0 body* ...))))
        ((_ name expr) 
         (identifier? #'name)
         (with-syntax ((CL/? (datum->syntax #'name 'case-lambda/?))
                       (L/? (datum->syntax #'name 'lambda/?)))
           #'(define name
               (let-syntax ((CL/? (syntax-rules ()
                                    ((_ . r) (case-lambda/?--meta name . r))))
                            (L/? (syntax-rules ()
                                   ((_ . r) (case-lambda/?--meta name r)))))
                 expr)))))))
  
  (define-syntax define/?/AV
    (lambda (stx)
      (syntax-case stx ()
        ((_ (fname . frmls) body0 body* ...)
         (identifier? #'fname)
         #'(define fname
             (AV-wrap fname fname
               (case-lambda/?--meta fname (frmls body0 body* ...)))))
        ((_ name expr)
         (identifier? #'name)
         #'(define/? name
             (AV-wrap name name
               expr))))))
)
