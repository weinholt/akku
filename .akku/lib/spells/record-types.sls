#!r6rs #!r6rs
;;; record-types.sls --- Record types.

;; Copyright (C) 2009, 2015 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(library (spells record-types)
  (export define-record-type*
          define-functional-fields
          define-record-discloser)
  (import (rnrs)
          (for (spells record-types expand-drt) expand))

  (define-syntax define-record-type*
    expand-define-record-type*)

  (define-syntax define-functional-fields
    expand-define-functional-fields)

  (define-syntax define-record-discloser
    (syntax-rules ()
      ((define-record-discloser type proc)
       (begin))))

)
