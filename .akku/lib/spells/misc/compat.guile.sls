#!r6rs #!r6rs
;;; compat.guile.sls --- Misc functions, Guile compatibility

;; Copyright (C) 2010, 2012 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(library (spells misc compat)
  (export sleep-seconds
          scheme-implementation)
  (import (rnrs base)
          (only (guile) usleep))

  (define (sleep-seconds t)
    (usleep (+ (* (exact (truncate t)) #e1e+6)
               (mod (exact (round (* t #e1e+6))) #e1e+6))))
  
  (define (scheme-implementation) 'guile))
