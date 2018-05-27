#!r6rs #!r6rs
;;; error.sls --- Error handling utilities.

;; Copyright (C) 2009, 2015 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

;;@ @uref{http://srfi.schemers.org/srfi-23/srfi-23.html, SRFI 23}
;;  compatability library.
(library (spells error)
  (export error error-who make-error-signaller)
  (import 
    (rename (rnrs base) (error rnrs:error))
    (srfi :39 parameters))
  
  (define error-who (make-parameter #f))
  
  (define (error . args)
    (apply rnrs:error (error-who) args))

  (define (make-error-signaller who)
    (lambda args
      (apply rnrs:error who args))))
