#!r6rs #!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl macro-utils fib p-ctxt)
  (export
    p-ctxt)
  (import
    (for (prefix (only (rnrs base) define) p-)
         (meta -1) (meta 0))
    (for (prefix (only (rnrs syntax-case) syntax) p-)
         (meta -1) (meta 0)))

  (p-define p-ctxt (p-syntax here))
)
