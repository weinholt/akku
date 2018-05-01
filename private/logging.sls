#!r6rs
;; Copyright (C) 2009, 2010, 2011, 2015 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;; SPDX-License-Identifier: LicenseRef-BSD-3-Clause-New-Style

(library (akku private logging)
  (export
    make-logger
    logger:akku
    make-fmt-log)
  (import
    (rnrs (6))
    (spells logging)
    (wak fmt))

(define logger:akku (make-logger root-logger 'akku))

(define (make-fmt-log parent level)
  (let ((log (make-log parent level)))
    (lambda formats
      (log (lambda (port) (apply fmt port formats)))))))
