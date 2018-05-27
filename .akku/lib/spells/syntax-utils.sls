#!r6rs #!r6rs
;;; syntax-utils.sls --- Helpers for syntax-case

;; Copyright (C) 2009, 2015 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(library (spells syntax-utils)
  (export identifier-append
          symbolic-identifier=?)
  (import (rnrs))

  (define (identifier-append k . parts)
    (datum->syntax
     k
     (string->symbol (apply string-append
                            (map (lambda (x)
                                   (cond ((string? x) x)
                                         ((identifier? x)
                                          (symbol->string (syntax->datum x)))
                                         (else (symbol->string x))))
                                 parts)))))

  (define (symbolic-identifier=? x y)
    (eq? (if (identifier? x) (syntax->datum x) x)
         (if (identifier? y) (syntax->datum y) y)))
  
  )
