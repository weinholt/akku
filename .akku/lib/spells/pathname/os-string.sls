#!r6rs #!r6rs
;;; os-string.sls --- Operating-system string abstraction.

;; Copyright (C) 2008-2009, 2015 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;; This is the generic file, which assumes that OS strings are just
;; plain strings. If the host implementation handles these
;; differently, a dialect-specific implementation must be provided.

;;; Code:

(library (spells pathname os-string)
  (export os-string?
          os-string->string)
  (import (rnrs base))

  (define os-string? string?)
  (define (os-string->string os-string)
    os-string)

  )
