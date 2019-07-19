#!r6rs
;; Akku.scm wrote this file based on "chibi-match-0.7.3/chibi/match.sld"

(library (chibi match)
  (export match match-lambda match-lambda* match-let
    match-letrec match-let*)
  (import (scheme base))
  (include "match/match.scm"))
