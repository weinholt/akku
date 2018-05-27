#!r6rs ;;; compat.ikarus.sls --- GC-interacting procedures for Ikarus.

;; Copyright (C) 2009 Andreas Rottmann <a.rottmann@gmx.at>

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
          (only (chezscheme)
                weak-cons weak-pair? bwp-object? make-guardian
                collect))

  (define (make-weak-cell obj)
    (weak-cons obj #f))

  (define (weak-cell? thing)
    (weak-pair? thing))

  (define (weak-cell-ref weak-cell)
    (let ((obj (car weak-cell)))
      (and (not (bwp-object? obj)) obj)))

  (define (make-reaper proc)
    (let ((guardian (make-guardian)))
      (case-lambda
        ((object)
         (guardian object))
        (()
         (let ((object (guardian)))
           (if object
               (proc object)
               #f)))))))
