#!r6rs #!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl predicates)
  (export
    not? and? or? xor?
    non-negative-integer?
    exact-non-negative-integer?
    positive-integer?
    exact-positive-integer?
    exact-integer?
    list-of?
    #;datum?
    pairwise?
    symbol<?
    name=?
    non-empty-string?
    char-line-ending?)
  (import
    (rnrs)
    (only (xitomatl conditionals) xor))

  (define (not? pred)
    (lambda (x)
      (not (pred x))))

  (define and?
    (case-lambda
      ((pred0 pred1)
       (lambda (x) (and (pred0 x) (pred1 x))))
      ((pred0 pred1 pred2)
       (lambda (x) (and (pred0 x) (pred1 x) (pred2 x))))
      (preds
       (lambda (x)
         (or (null? preds)
             (let loop ((preds preds))
               (if (null? (cdr preds))
                 ((car preds) x)  ;; tail call
                 (and ((car preds) x)
                      (loop (cdr preds))))))))))

  (define or?
    (case-lambda
      ((pred0 pred1)
       (lambda (x) (or (pred0 x) (pred1 x))))
      ((pred0 pred1 pred2)
       (lambda (x) (or (pred0 x) (pred1 x) (pred2 x))))
      (preds
       (lambda (x)
         (and (pair? preds)
              (let loop ((preds preds))
                (if (null? (cdr preds))
                  ((car preds) x)  ;; tail call
                  (or ((car preds) x)
                      (loop (cdr preds))))))))))

  (define xor?
    ;; NOTE: Does not tail-call the last predicate.
    (case-lambda
      ((pred0 pred1)
       (lambda (x) (xor (pred0 x) (pred1 x))))
      ((pred0 pred1 pred2)
       (lambda (x) (xor (pred0 x) (pred1 x) (pred2 x))))
      (preds
       (lambda (x)
         (let loop ((preds preds) (r #F))
           (if (null? preds)
             r
             (let ((v ((car preds) x)))
               (if v
                 (and (not r)
                      (loop (cdr preds) v))
                 (loop (cdr preds) r)))))))))
  
  (define (non-negative-integer? x)
    (and (integer? x) (not (negative? x))))

  (define (exact-non-negative-integer? x)
    (and (integer? x) (exact? x) (not (negative? x))))
  
  (define (positive-integer? x)
    (and (integer? x) (positive? x)))
  
  (define (exact-positive-integer? x)
    (and (integer? x) (exact? x) (positive? x)))
  
  (define (exact-integer? x)
    (and (integer? x) (exact? x)))
  
  (define (list-of? pred)
    (letrec ((list-of?-pred
              (lambda (x)
                (if (pair? x)
                  (and (pred (car x))
                       (list-of?-pred (cdr x)))
                  (null? x)))))
      list-of?-pred))
  
  #;(define (datum? x)
    ;; The naive implementation cannot handle cyclic structures.
    ;; How to do this..?
    )

  (define pairwise?
    ;; Make a predicate which tests if all its arguments are pairwise true
    ;; for a given binary predicate.  0 and 1 arguments are always considered
    ;; true; e.g.: ((pairwise? <)) => #T and ((pairwise? =) 42) => #T.
    ;; The optional 2nd argument is an arbitrary procedure that takes 1
    ;; argument, and it is applied to each element once and must return a value
    ;; to use with the binary predicate, or raise an exception; this procedure
    ;; is useful for efficiently type-checking elements and/or transforming them.
    (case-lambda
      ((binary-pred)
       (pairwise? binary-pred #F))
      ((binary-pred proc)
       (let ((next (if proc
                     (lambda (l) (proc (car l)))
                     car)))
         (lambda args
           (or (null? args)
               (let ((x (next args)))
                 (let loop ((x x) (r (cdr args)))
                   (or (null? r)
                       (let ((y (next r)))
                         (and (binary-pred x y)
                              (loop y (cdr r)))))))))))))

  (define symbol<?
    (pairwise? string<?
     (lambda (x)
       (if (symbol? x)
         (symbol->string x)
         (assertion-violation 'symbol<? "not a symbol" x)))))

  (define name=?
    (pairwise? string=?
     (lambda (x) 
       (cond ((identifier? x) (symbol->string (syntax->datum x)))
             ((symbol? x) (symbol->string x))
             ((string? x) x)
             (else (assertion-violation 'name=? 
                    "not an identifier, symbol, or string" x))))))
  
  (define (non-empty-string? x)
    (and (string? x) (positive? (string-length x))))
  
  (define (char-line-ending? c)
    (and (memv c '(#\xa #\xd #\x85 #\x2028))  ;; correct? everything it should be?
         #T))

)
