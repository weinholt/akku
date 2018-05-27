#!r6rs #!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl records)
  (export
    record-type-fields
    record-type-accessors
    record-type-mutators)
  (import
    (rnrs))

  (define (record-type-things rtd proc)
    (let loop ((rtd rtd) (things '()))
      (if rtd
        (loop (record-type-parent rtd)
              (let ((fns (vector->list (record-type-field-names rtd))))
                (let loop ((i (- (length fns) 1))
                           (fns (reverse fns))
                           (things things))
                  (if (null? fns)
                    things
                    (loop (- i 1)
                          (cdr fns)
                          (cons (proc rtd (car fns) i)
                                things))))))
        things)))

  (define (record-type-fields rtd)
    (record-type-things rtd
     (lambda (rtd fn i) fn)))
    
  (define (record-type-accessors rtd)
    (record-type-things rtd
     (lambda (rtd fn i) (record-accessor rtd i))))
  
  (define (record-type-mutators rtd)
    (record-type-things rtd
     (lambda (rtd fn i)
       (and (record-field-mutable? rtd i)
            (record-mutator rtd i)))))
)
