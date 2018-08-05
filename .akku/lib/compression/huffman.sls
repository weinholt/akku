#!r6rs ;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2009, 2010, 2012, 2017 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT

;; Permission is hereby granted, free of charge, to any person obtaining a
;; copy of this software and associated documentation files (the "Software"),
;; to deal in the Software without restriction, including without limitation
;; the rights to use, copy, modify, merge, publish, distribute, sublicense,
;; and/or sell copies of the Software, and to permit persons to whom the
;; Software is furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.
#!r6rs

;; Procedures for David Huffman's codes. These are suitable for use
;; with DEFLATE.

(library (compression huffman)
  (export reconstruct-codes
          canonical-codes->simple-lookup-table
          canonical-codes->lookup-table
          get-next-code)
  (import (except (rnrs) fxreverse-bit-field)
          (compression bitstream)
          (compression private compat))

  (define-syntax trace
    (syntax-rules ()
      #;
      ((_ . args)
       (begin
         (for-each display (list . args))
         (newline)))
      ((_ . args) (begin 'dummy))))

  (define-syntax revtable
    (lambda (x)
      (define (rev v start end)
        (do ((i start (fx+ i 1))
             (ret 0 (if (fxbit-set? v i)
                        (fxior ret (fxarithmetic-shift-left 1 (fx- (fx- end i) 1)))
                        ret)))
            ((fx=? i end)
             (fxior (fxarithmetic-shift-left ret start)
                    (fxcopy-bit-field v start end 0)))))
      (syntax-case x ()
        ((table bits)
         (let* ((bits (syntax->datum #'bits))
                (len (fxarithmetic-shift-left 1 bits)))
           (do ((v (make-vector len))
                (i 0 (+ i 1)))
               ((= i len)
                (with-syntax ((t v))
                  #'(begin 't)))
             (vector-set! v i (rev i 0 bits))))))))

  (define (reverse-bits i end)
    (define rev9 (revtable 9))
    (if (fx<=? end 9)
        (fxarithmetic-shift-right (vector-ref rev9 i) (fx- 9 end))
        (fxreverse-bit-field i 0 end)))

  ;; If you have a canonical Huffman tree, with a known alphabet, then
  ;; all that is needed to reconstruct the tree is the length of each
  ;; symbol in the alphabet. This procedure takes a list of ((symbol .
  ;; bit-length) ...) and computes the codes.
  (define (reconstruct-codes sym< syms+lens)
    ;; The canonical codes are described in RFC 1951 section 3.2.2.
    ;; Don't try to read their code though...
    (define (sort-by-length x)
      (list-sort (lambda (x y) (< (cdr x) (cdr y))) x))
    (define (sort-by-alphabet x)
      (list-sort (lambda (x y) (sym< (car x) (car y))) x))
    (let lp ((code 0)
             (syms+lens (sort-by-length syms+lens))
             (ret '()))
      (let ((sym+len+code (list (caar syms+lens) (cdar syms+lens) code)))
        (if (null? (cdr syms+lens))
            (sort-by-alphabet (cons sym+len+code ret))
            (lp (bitwise-arithmetic-shift-left (+ code 1)
                                               (- (cdadr syms+lens)
                                                  (cdar syms+lens)))
                (cdr syms+lens)
                (cons sym+len+code ret))))))

  ;; (reconstruct-codes char<? '((#\A . 3) (#\B . 3) (#\C . 3) (#\D . 3) (#\E . 3) (#\F . 2) (#\G . 4) (#\H . 4)))

  ;; (define (depth x)
  ;;   (if (pair? x)
  ;;       (+ 1 (max (depth (car x)) (depth (cdr x))))
  ;;       0))

  ;; freqs is a list of (frequency . value) pairs. At least two pairs
  ;; are needed.
  ;; (define (frequencies->huffman-tree < freqs)
  ;;   ;; TODO: linear time?
  ;;   (define (sort freqs)
  ;;     (list-sort (lambda (x y) (< (car x) (car y)))
  ;;                freqs))
  ;;   (if (null? (cdr freqs))
  ;;       (cdar freqs)
  ;;       (let* ((freqs (sort freqs))
  ;;              (node (cond ((< (depth (cdar freqs)) (depth (cdadr freqs))) ;help make it unique
  ;;                           (cons (cdar freqs) (cdadr freqs)))
  ;;                          (else
  ;;                           (cons (cdadr freqs) (cdar freqs)))))
  ;;              (weight (+ (caar freqs) (caadr freqs))))
  ;;         (frequencies->huffman-tree < (cons (cons weight node)
  ;;                                            (cddr freqs))))))

  ;; (define (flatten-huffman-tree tree)
  ;;   ;; Turns a binary tree into a list of (symbol length code).
  ;;   (define (flatten t len code)
  ;;     (cond
  ;;       ((pair? t)
  ;;        (append (flatten (car t) (+ len 1) (bitwise-arithmetic-shift-left code 1))
  ;;                (flatten (cdr t) (+ len 1) (bitwise-ior 1 (bitwise-arithmetic-shift-left code 1)))))
  ;;       (else
  ;;        (list (list t len code)))))
  ;;   (flatten tree 0 0))

  ;; Turns a Huffman tree into a list of (symbol length code), where
  ;; the code is the canonical code.
  ;; (define (huffman-tree->canonical-codes sym<? tree)
  ;;   (reconstruct-codes                  ;Assign canonical codes
  ;;    sym<?
  ;;    (map (lambda (s/l/c)
  ;;           ;; Take the symbols and their lenghts
  ;;           (cons (car s/l/c) (cadr s/l/c)))
  ;;         ;; Sort first by code length and then by the symbol.
  ;;         (list-sort (lambda (x y)
  ;;                      (if (= (cadr x) (cadr y))
  ;;                          (sym<? (car x) (car y))
  ;;                          (< (cadr x) (cadr y))))
  ;;                    (flatten-huffman-tree tree)))))

  ;; (flatten-huffman-tree '((#\F #\A . #\B) (#\C . #\D) #\E #\G . #\H))

  ;; (canonicalize-huffman-tree char<? '((#\F #\D . #\C) (#\B . #\A) #\E #\H . #\G))


  ;; (graph '((#\F #\A . #\B) (#\C . #\D) #\E #\G . #\H))

  ;; This takes a list of canonical codes ((symbol bit-length code)
  ;; ...) and constructs a lookup table. It's a one-level table (so
  ;; don't use this with a giant code). Let M be the maximum bit
  ;; length in the table, then this table is 2^M large, and you're
  ;; supposed to peek M bits into the stream. Use the peek'd value as
  ;; an index into the table, and the entry will tell you how many
  ;; bits belong to the symbol, and what the symbol is.
  (define (canonical-codes->simple-lookup-table codes)
    (let ((maxlen (fold-right max 0 (map cadr codes))))
      (do ((t (make-vector (fxarithmetic-shift-left 1 maxlen) #f))
           (codes codes (cdr codes)))
          ((null? codes)
           (cons maxlen t))
        (let* ((code (car codes))
               (symbol (car code)) (bitlen (cadr code)) (bits (caddr code))
               (translation (cons bitlen symbol)))
          (let* ((start (fxarithmetic-shift-left bits (- maxlen bitlen)))
                 (end (fxior start (- (fxarithmetic-shift-left 1 (- maxlen bitlen)) 1))))
            (do ((i start (fx+ i 1)))
                ((fx>? i end))
              (vector-set! t (fxreverse-bit-field i 0 maxlen) translation)))))))

  ;; (canonical-codes->simple-lookup-table
  ;;  '((#\A 3 2) (#\B 3 3) (#\C 3 4) (#\D 3 5) (#\E 3 6) (#\F 2 0) (#\G 4 14) (#\H 4 15)))

  ;; Uses two tables, as described here: http://www.gzip.org/algorithm.txt
  (define (canonical-codes->lookup-table codes)
    (define maxlen
      ;; Length of the first table. 9 is a sweet spot, but causes
      ;; trouble because it can consume a byte too much in lookahead
      ;; (like at the end of a gzip stream).
      (let lp ((m 1) (codes codes))
        (if (null? codes)
            m
            (let ((bitlen (cadar codes)))
              (if (fx>=? bitlen 8) 9
                  (lp (max m bitlen)
                      (cdr codes)))))))
    (define (findmax codes prefix)
      ;; Find the maximum code length for codes where the first
      ;; `maxlen' bits are equal to the prefix.
      (let lp ((m 0) (codes codes))
        (if (null? codes)
            m
            (let ((bitlen (cadar codes))
                  (bits (caddar codes)))
              (cond ((fx>? maxlen bitlen) (lp m (cdr codes)))
                    ((fx=? (fxarithmetic-shift-right bits (fx- bitlen maxlen))
                           prefix)
                     (lp (max m (fx- bitlen maxlen)) (cdr codes)))
                    (else (lp m (cdr codes))))))))
    (define (fill! t bits maxlen bitlen translation)
      (let* ((start (fxarithmetic-shift-left bits (fx- maxlen bitlen)))
             (end (fxior start (fx- (fxarithmetic-shift-left 1 (fx- maxlen bitlen)) 1))))
        (trace "#;start: #b" (string-pad (number->string start 2) bitlen #\0))
        (trace "#;end:   #b" (string-pad (number->string end 2) bitlen #\0))
        (do ((i start (fx+ i 1)))
            ((fx>? i end))
          (trace `(set! ,i ,translation))
          (vector-set! t (reverse-bits i maxlen) translation))))
    (do ((t (make-vector (fxarithmetic-shift-left 1 maxlen) #f))
         (codes codes (cdr codes)))
        ((null? codes)
         (cons maxlen t))
      (let* ((code (car codes))
             (symbol (car code)) (bitlen (cadr code)) (bits (caddr code)))
        (trace "#;symbol: " symbol " #;bitlen: "bitlen " #;bits: #b"
               (string-pad (number->string bits 2) bitlen #\0))
        (if (fx<=? bitlen maxlen)
            (fill! t bits maxlen bitlen (cons bitlen symbol))
            (let* ((bitlen (fx- bitlen maxlen)) ;bitlength in table 2
                   (i (fxarithmetic-shift-right bits bitlen))
                   (ri (reverse-bits i maxlen))
                   (t2 (cond ((vector-ref t ri) => cdr)
                             (else ;; make a second-level table
                              (let* ((maxlen* (findmax codes i))
                                     (v (make-vector (fxarithmetic-shift-left 1 maxlen*) #f))
                                     (t2 (cons maxlen* v)))
                                (trace `(set! ,i table))
                                (vector-set! t ri (cons maxlen t2))
                                t2))))
                   (bits (fxand bits (fx- (fxarithmetic-shift-left 1 bitlen) 1))))
              (fill! (data t2) bits (len t2) bitlen (cons bitlen symbol)))))))

  ;; (flatten-huffman-tree '((1 4 . 3) (5 7 10 . 9) 2 (6 . 11) 8 12 . 13))

  (define len car)
  (define data cdr)

  ;; This lookup code is the companion of the procedure above.
  (define (get-next-code br table)
    (let ((code (lookahead-bits br (len table))))
      (let ((translation (vector-ref (data table) code)))
        (trace "code: " (string-pad (number->string code 2) (len translation) #\0) " => " (data translation))
        (get-bits br (len translation))
        (if (pair? (data translation))
            (get-next-code br (data translation))
            (data translation))))))
