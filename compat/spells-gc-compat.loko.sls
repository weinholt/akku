#!r6rs
; compat.mosh.sls - (spells gc) compatibility layer for Mosh
; copyright: 2011 David Banks <amoebae@gmail.com>; license: BSD-3-clause

(library (spells gc compat)
  (export make-weak-cell weak-cell-ref weak-cell?
          make-reaper
          collect)
  (import (rnrs)
          (spells misc))

  ; Mosh doesn't have any weak reference support at the moment.
  ; Fake it out.

  (define-record-type weak-cell
    (fields object))

  (define weak-cell-ref weak-cell-object)

  (define (make-reaper proc)
    (case-lambda
      ((obj) (unspecific))
      (()    #f)))

  (define (collect)
    (unspecific))
)
