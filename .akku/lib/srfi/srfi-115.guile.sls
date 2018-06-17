#!r6rs ;; Copyright notices may be found in "%3a115/regexp.sls"
;; This file was written by Akku.scm
(library (srfi srfi-115)
  (export regexp regexp? valid-sre? rx regexp->sre
   char-set->sre regexp-matches regexp-matches? regexp-search
   regexp-replace regexp-replace-all regexp-fold regexp-extract
   regexp-split regexp-partition regexp-match?
   regexp-match-count regexp-match-submatch
   regexp-match-submatch/list regexp-match-submatch-start
   regexp-match-submatch-end regexp-match->list
   regexp-match->sexp)
  (import
    (rename
      (except (rnrs) define-record-type string-ci-hash string-hash
        error)
      (exists any))
    (only (rnrs r5rs) quotient) (rnrs mutable-pairs)
    (srfi :9 records) (srfi :14 char-sets) (srfi :23 error)
    (srfi :69 basic-hash-tables) (srfi :115 regexp boundary)
    (srfi private include))
  (define %char-set:letter
    (char-set-intersection char-set:ascii char-set:letter))
  (define %char-set:lower-case
    (char-set-intersection char-set:ascii char-set:lower-case))
  (define %char-set:upper-case
    (char-set-intersection char-set:ascii char-set:upper-case))
  (define %char-set:digit
    (char-set-intersection char-set:ascii char-set:digit))
  (define %char-set:letter+digit
    (char-set-intersection
      char-set:ascii
      char-set:letter+digit))
  (define %char-set:punctuation
    (char-set-intersection char-set:ascii char-set:punctuation))
  (define %char-set:symbol
    (char-set-intersection char-set:ascii char-set:symbol))
  (define %char-set:graphic
    (char-set-intersection char-set:ascii char-set:graphic))
  (define %char-set:whitespace
    (char-set-intersection char-set:ascii char-set:whitespace))
  (define %char-set:printing
    (char-set-intersection char-set:ascii char-set:printing))
  (define %char-set:iso-control
    (char-set-intersection char-set:ascii char-set:iso-control))
  (define (string-start-arg s o)
    (if (pair? o) (string-index->cursor s (car o)) 0))
  (define (string-end-arg s o)
    (if (pair? o)
        (string-index->cursor s (car o))
        (string-length s)))
  (define string-cursor? integer?)
  (define string-cursor=? =)
  (define string-cursor<? <)
  (define string-cursor<=? <=)
  (define string-cursor>? >)
  (define string-cursor>=? >=)
  (define string-cursor-ref string-ref)
  (define (string-cursor-next s i) (+ i 1))
  (define (string-cursor-prev s i) (- i 1))
  (define substring-cursor substring)
  (define (string-cursor->index str off) off)
  (define (string-index->cursor str i) i)
  (define (string-concatenate ls) (apply string-append ls))
  (define (string-concatenate-reverse ls)
    (string-concatenate (reverse ls)))
  (include/resolve ("srfi" "%3a115") "regexp.scm"))
