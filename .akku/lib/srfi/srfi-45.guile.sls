#!r6rs ;; Copyright notices may be found in "%3a45/lazy.sls"
;; This file was written by Akku.scm
(library (srfi srfi-45)
  (export delay lazy force eager)
  (import (rnrs base) (rnrs records syntactic))
  (define-record-type promise (fields (mutable val)))
  (define-record-type value
    (fields (mutable tag) (mutable proc)))
  (define-syntax lazy
    (syntax-rules ()
      [(lazy exp)
       (make-promise (make-value 'lazy (lambda () exp)))]))
  (define (eager x) (make-promise (make-value 'eager x)))
  (define-syntax delay
    (syntax-rules () [(delay exp) (lazy (eager exp))]))
  (define (force promise)
    (let ([content (promise-val promise)])
      (case (value-tag content)
        [(eager) (value-proc content)]
        [(lazy)
         (let* ([promise* ((value-proc content))]
                [content (promise-val promise)])
           (if (not (eqv? (value-tag content) 'eager))
               (begin
                 (value-tag-set!
                   content
                   (value-tag (promise-val promise*)))
                 (value-proc-set!
                   content
                   (value-proc (promise-val promise*)))
                 (promise-val-set! promise* content)))
           (force promise))]))))
