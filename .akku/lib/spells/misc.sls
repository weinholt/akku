#!r6rs #!r6rs
;;; misc.sls --- Misc stuff.

;; Copyright (C) 2009, 2015 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

;;@ Stuff that doesn't fit somewhere else.
(library (spells misc)
  (export identity
          compose
          unspecific
          sleep-seconds
          sort-list
          and-map
          or-map
          and=>
          topological-sort
          scheme-implementation)
  (import (rnrs base)
          (rnrs lists)
          (rnrs io simple)
          (rnrs sorting)
          (rnrs hashtables)
          (spells include)
          (spells misc compat))

  (include-file ((spells private) misc)))
