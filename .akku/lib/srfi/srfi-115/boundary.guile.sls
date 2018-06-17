#!r6rs ;; Copyright notices may be found in "%3a115/regexp/boundary.sls"
;; This file was written by Akku.scm
(library (srfi srfi-115 boundary)
  (export char-set:regional-indicator char-set:extend-or-spacing-mark
    char-set:hangul-l char-set:hangul-v char-set:hangul-t
    char-set:hangul-lv char-set:hangul-lvt)
  (import (rnrs) (srfi :14 char-sets) (srfi private include))
  (define (immutable-char-set cs) cs)
  (include/resolve ("srfi" "%3a115" "regexp") "boundary.scm"))
