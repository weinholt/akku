#!r6rs
;;; compat.loko.sls --- include compatibility for Loko Scheme

;; Copyright (C) 2009, 2010, 2011 Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the MIT/X11 license.

;; You should have received a copy of the MIT/X11 license along with
;; this program. If not, see
;; <http://www.opensource.org/licenses/mit-license.php>.

;;; Commentary:

;;; Code:

(library (wak private include compat)
  (export stale-when
          read-annotated
          annotation?
          annotation-expression
          file-mtime
          merge-path
          (rename (library-directories library-search-paths)))
  (import (rnrs base)
          (rnrs io simple)
          (rnrs lists)
          (only (loko) library-directories)
          (srfi :170))

(define-syntax stale-when
  (syntax-rules ()
    ((_ conditition body ...)
     (begin body ...))))

(define (read-annotated port)
  (read port))

(define (annotation? thing)
  #f)

(define (annotation-expression thing)
  thing)

(define (string-join lst sep)
  (if (null? lst)
    ""
    (fold-left (lambda (a b) (string-append a sep b)) (car lst) (cdr lst))))

(define (merge-path path origin)
  (let ([sep "/"])
    (string-append origin sep (string-join path sep))))

(define (file-mtime fn)
  (file-info:mtime (file-info fn)))
)
