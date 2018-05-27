#!r6rs #!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl conditions)
  (export
    make-argument-name-condition argument-name-condition? condition-argument-name
    make-predicate-expression-condition predicate-expression-condition? condition-predicate-expression
    make-port-position-condition port-position-condition? condition-port-position
    print-condition)
  (import
    (rnrs)
    (xitomatl conditions print-condition))  
      
  (define-condition-type &argument-name &condition
    make-argument-name-condition argument-name-condition?
    (n condition-argument-name))
      
  (define-condition-type &predicate-expression &condition
    make-predicate-expression-condition predicate-expression-condition?
    (p condition-predicate-expression))
  
  (define-condition-type &port-position &condition
    make-port-position-condition port-position-condition?
    (pos condition-port-position))
)
