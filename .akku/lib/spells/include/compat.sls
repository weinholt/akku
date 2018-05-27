#!r6rs #!r6rs
;;; compat.sls --- include compatibility

;; Copyright (C) 2009, 2015 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(library (spells include compat)
  (export stale-when
          read-annotated
          annotation?
          annotation-expression)
  (import (rnrs base)
          (rnrs io simple))

  (define-syntax stale-when
    (syntax-rules ()
      ((_ conditition body ...)
       (begin body ...))))

  (define (read-annotated port)
    (read port))

  (define (annotation? thing)
    #f)
  
  (define (annotation-expression thing)
    thing)
  
)
