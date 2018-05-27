#!r6rs #!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl macro-utils fib ctxt)
  (export
    ctxt)
  (import
    (for (only (rnrs base) define)
         (meta -1) (meta 0))
    (for (only (rnrs syntax-case) syntax)
         (meta -1) (meta 0)))

  (define ctxt #'here)
)
