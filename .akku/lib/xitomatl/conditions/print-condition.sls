#!r6rs #!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl conditions print-condition)
  (export
    print-condition)
  (import
    (rnrs)
    (only (xitomatl records) record-type-fields record-type-accessors)
    (only (xitomatl common) pretty-print))

  (define print-condition
    (case-lambda
      ((c)
       (print-condition c (current-output-port)))
      ((c p)
       ;; TODO: Nicely formated print-out like Ikarus does
       (define (info c)
         (let ((rtd (record-rtd c)))
           (cons (record-type-name rtd)
                 (map (lambda (f a) (list f (a c)))
                      (record-type-fields rtd)
                      (record-type-accessors rtd)))))
       (display "Condition:\n" p)
       (pretty-print (map info (simple-conditions c)) p))))
)
