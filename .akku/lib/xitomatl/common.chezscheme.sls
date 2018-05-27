#!r6rs ;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl common)
  (export
    add1 sub1
    format printf fprintf pretty-print
    gensym
    time
    with-input-from-string with-output-to-string
    system
    ;; TODO: add to as needed/appropriate
    )
  (import
    (chezscheme))
)
