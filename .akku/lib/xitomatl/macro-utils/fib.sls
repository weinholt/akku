#!r6rs #!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl macro-utils fib)
  (export
    free-identifier-bound?)
  (import
    (for (rnrs base)
         (meta 0))
    (for (only (rnrs control) unless)
         (meta 0))
    (for (rnrs syntax-case)
         (meta 0))
    (for (only (rnrs base) define)
         (meta -1))
    (for (only (rnrs syntax-case) syntax)
         (meta -1))
    (for (xitomatl macro-utils fib ctxt)
         (meta -1) (meta 0))
    (for (xitomatl macro-utils fib p-ctxt)
         (meta -1) (meta 0)))

  (define (free-identifier-bound? id)
    (unless (identifier? id)
      (assertion-violation 'free-identifier-bound? "not an identifier" id))
    ;; Thanks to Aziz Ghuloum for thinking of these tricks.
    (or (free-identifier=? id #'define)
        (free-identifier=? id #'syntax)
        (free-identifier=? id #'ctxt)
        (free-identifier=? id #'p-ctxt)
        (let ((sym (syntax->datum id)))
          (not (or (free-identifier=? id (datum->syntax ctxt sym))
                   (free-identifier=? id (datum->syntax p-ctxt sym)))))))
)
