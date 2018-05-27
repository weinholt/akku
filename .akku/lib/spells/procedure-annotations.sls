#!r6rs #!r6rs
;;; procedure-annotations.sls --- Attach data to procedures.

;; Copyright (C) 2009, 2015 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

;;@ Procedure annotations.
(library (spells procedure-annotations)
  (export annotate-procedure
          procedure-annotation)
  (import (rnrs base)
          (spells define-values))

  ;; Naive, portable implementation
  (define-values (annotate-procedure procedure-annotation)
    (let ((tag (list 'procedure-annotation)))
      (values
       (lambda (proc value)
         (lambda args
           (if (and (not (null? args))
                    (null? (cdr args))
                    (eq? (car args) tag))
               value
               (apply proc args))))
       (lambda (proc)
         (proc tag))))))
