#!r6rs
;; Copyright notices may be found in "chibi-match-0.7.3/chibi/match.sld"
;; This file was written by Akku.scm
(library (chibi match)
  (export match match-lambda match-lambda* match-let
    match-letrec match-let*)
  (import (scheme base))
  (include "match/match.scm"))
