#!r6rs ;; Copyright notices may be found in "%3a2/and-let%2a.sls"
;; This file was written by Akku.scm
(library (srfi srfi-2)
  (export and-let*)
  (import (rnrs))
  (define-syntax and-let*
    (syntax-rules () [(_ . r) (and-let*-core #t . r)]))
  (define-syntax and-let*-core
    (lambda (stx)
      (syntax-case stx ()
        [(kw _ ((var expr) . c) . b)
         #'(let ([var expr]) (and var (kw var c . b)))]
        [(kw last ((expr) . c) . b) #'(kw last ((t expr) . c) . b)]
        [(kw _ (id . c) . b)
         (identifier? #'id)
         #'(and id (kw id c . b))]
        [(_ last ()) #'last]
        [(_ _ () . b) #'(let () . b)]))))
