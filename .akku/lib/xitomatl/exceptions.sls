#!r6rs #!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl exceptions)
  (export
    catch reraise
    warning warning/conditions assertion-violation/conditions error/conditions
    syntax-violation/conditions
    print-exception)
  (import
    (rnrs)
    (for (only (xitomatl macro-utils) with-syntax* gen-temp) expand)
    (only (xitomatl conditions) print-condition)
    (only (xitomatl common) pretty-print))
  
  (define-syntax catch
    (lambda (stx)
      (syntax-case stx ()
        ((_ var (in-clause ...) expr0 expr ...)
         (with-syntax* 
             ((catch-k (gen-temp))
              ((out-clause ...) 
               (map (lambda (ic)
                      (syntax-case ic (=>)
                        ((test => proc) 
                         #'(test => (lambda (t) (catch-k (lambda () (proc t))))))
                        ((test)
                         #'(test => (lambda (t) (catch-k (lambda () t)))))
                        ((test/else expr ...)
                         #'(test/else (catch-k (lambda () expr ...))))))
                    #'(in-clause ...))))
           #`((call/cc
                (lambda (catch-k)
                  (lambda ()
                    (with-exception-handler
                      (lambda (var)
                        #,(and (positive? (length #'(out-clause ...)))
                               #'(cond out-clause ...))
                        (reraise var))
                      (lambda ()
                        expr0 expr ...)))))))))))
  
  (define (reraise obj)
    ;; If R7RS makes exceptions discernable as continuable or not,
    ;; this will change to use raise or raise-continuable depending.
    (raise-continuable obj))
  
  (define (make-raiser/conditions raise-it make-main-condition)
    (lambda (who msg irrts . cndts)
      (raise-it 
       (apply condition
              (make-main-condition)
              (if who 
                (make-who-condition who)
                (condition))
              (make-message-condition msg)
              (make-irritants-condition irrts)
              cndts))))
  
  (define assertion-violation/conditions
    (make-raiser/conditions raise make-assertion-violation))
  
  (define error/conditions
    (make-raiser/conditions raise make-error))
  
  (define warning/conditions
    (make-raiser/conditions raise-continuable make-warning))
  
  (define (warning who msg . irrts)
    (warning/conditions who msg irrts))
  
  (define (syntax-violation/conditions who msg form subform . cndts)
    (with-exception-handler
      (lambda (ex) (raise (apply condition ex cndts)))
      (lambda () (syntax-violation who msg form subform))))

  (define print-exception 
    (case-lambda
      ((exn)
       (print-exception exn (current-output-port)))
      ((exn p)
       (display "Exception:\n" p)
       (if (condition? exn)
         (print-condition exn p)
         (pretty-print exn p)))))
)
