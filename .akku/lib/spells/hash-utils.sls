#!r6rs #!r6rs
;;; hash-utils.sls --- 

;; Copyright (C) 2009, 2015 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(library (spells hash-utils)
  (export hash-combine
          hash-fold)
  (import (rnrs base)
          (rnrs arithmetic fixnums))

(define hash-bits (- (fixnum-width) 1))
(define hash-mask (fxnot (fxarithmetic-shift -1 hash-bits)))

(define (hash-combine h1 h2)
  (fxxor (fxrotate-bit-field (fxand h1 hash-mask) 0 hash-bits 7)
         (fxrotate-bit-field (fxand h2 hash-mask) 0 hash-bits (- hash-bits 6))))

(define (hash-fold hasher initial-hash lst)
  (let loop ((hash initial-hash) (lst lst))
    (if (null? lst)
        hash
        (loop (hash-combine hash (hasher (car lst)))
              (cdr lst)))))

)
