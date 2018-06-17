#!r6rs ;; Copyright (c) 2018 Göran Weinholt. See the MIT-style license in the
;; file named LICENSE from the original collection this file is
;; distributed with.

(library (xitomatl common)
  (export
    add1 sub1
    format printf fprintf pretty-print
    gensym
    time
    with-input-from-string with-output-to-string
    system)
  (import
    (guile)
    (ice-9 pretty-print)
    (prefix (ice-9 format) ice-9:)
    (ice-9 time))

  (define (add1 x) (+ 1 x))
  (define (sub1 x) (- x 1))

  (define (format fmt-str . fmt-args)
    (apply ice-9:format #f fmt-str fmt-args))

  (define (printf fmt-str . fmt-args)
    (apply ice-9:format #t fmt-str fmt-args))

  (define (fprintf port fmt-str . fmt-args)
    (apply ice-9:format port fmt-str fmt-args)))
