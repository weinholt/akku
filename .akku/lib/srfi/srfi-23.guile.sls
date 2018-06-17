#!r6rs ;; Copyright notices may be found in "%3a23/error.sls"
;; This file was written by Akku.scm
(library (srfi srfi-23)
  (export error)
  (import (rename (rnrs base) (error rnrs:error)))
  (define (error . args) (apply rnrs:error #f args)))
