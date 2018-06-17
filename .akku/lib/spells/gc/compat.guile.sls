#!r6rs ;;; compat.guile.sls --- GC-interacting procedures for Guile

;; Copyright (C) 2010, 2015 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:


(library (spells gc compat)
  (export make-weak-cell weak-cell-ref weak-cell?
          make-reaper
          collect)
  (import (rnrs base)
          (rnrs control)
          (ice-9 weak-vector)
          (only (guile) make-guardian gc))

  (define (make-weak-cell obj)
    ;; Guile seems to have issues with `weak-vector', so we do it this
    ;; way
    (let ((result (make-weak-vector 1)))
      (weak-vector-set! result 0 obj)
      result))

  (define (weak-cell? thing)
    (weak-vector? thing))

  (define (weak-cell-ref weak-cell)
    (weak-vector-ref weak-cell 0))

  (define (make-reaper proc)
    (let ((guardian (make-guardian)))
      (case-lambda
        ((object)
         (guardian object))
        (()
         (let ((object (guardian)))
           (if object
               (proc object)
               #f))))))

  (define collect gc)

  )
