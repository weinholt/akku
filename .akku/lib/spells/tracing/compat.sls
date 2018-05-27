#!r6rs #!r6rs
;;; Code taken from Ikarus Scheme, and tweaked.
;;;
;;; Ikarus Scheme -- A compiler for R6RS Scheme.
;;; Copyright (C) 2006,2007,2008,2015  Abdulaziz Ghuloum
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License version 3 as
;;; published by the Free Software Foundation.

(library (spells tracing compat)
  (export trace-lambda trace-define)
  (import (rnrs base)
          (rnrs control)
          (rnrs io simple)
          (srfi :8 receive))

  (define display-prefix
    (lambda (ls t)
      (unless (null? ls)
        (display (if t "|" " "))
        (display-prefix (cdr ls) (not t)))))

  (define display-trace
    (lambda (k* v)
      (display-prefix k* #t)
      (write v)
      (newline)))


  (define k* '())

  (define (make-traced-procedure name proc)
    (lambda args
      (display-trace (cons 1 k*) (cons name args))
      (dynamic-wind
        (lambda () (set! k* (cons 1 k*)))
        (lambda ()
          (call-with-values
              (lambda ()
                (apply proc args))
            (lambda v*
              (display-prefix k* #t)
              (unless (null? v*)
                (write (car v*))
                (let loop ((v* (cdr v*)))
                  (unless (null? v*)
                    (write-char #\space)
                    (write (car v*))
                    (loop (cdr v*)))))
              (newline)
              (apply values v*))))
        (lambda () (set! k* (cdr k*))))))

  (define-syntax trace-define
    (syntax-rules ()
      ((trace-define (name . args) body ...)
       (define name (trace-lambda name args
                      body ...)))))

  (define-syntax trace-lambda
    (syntax-rules ()
      ((trace-lambda name args body ...)
       (make-traced-procedure 'name
                              (lambda args body ...)))))

  )
