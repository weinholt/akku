#!r6rs ;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2011, 2012, 2017 Göran Weinholt <goran@weinholt.se>
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

;; Decoder for Igor Pavlov's LZMA format.

;; This hasn't been tested with LZMA1 streams.

(library (compression lzma)
  (export lzma-decode-chunk)
  (import (rnrs)
          (compression sliding-buffer)
          (compression private common))

  (define-syntax trace
    (syntax-rules ()
      #;
      ((_ . args)
       (begin
         (for-each display (list . args))
         (newline)))
      ((_ . args) (begin 'dummy))))

  (define (lzma-decode-chunk in dictionary usize lc lp pb position)
    (define asl bitwise-arithmetic-shift-left)
    (define asr bitwise-arithmetic-shift-right)
    (define fxasl fxarithmetic-shift-left)
    (define fxasr fxarithmetic-shift-right)
    ;; Range coding. This can use fixnums if fixnum-width can fit
    ;; 32-bit unsigned integers.
    (define rc-top-bits 24)
    (define rc-model-total-bits 11)
    (define rc-bit-model-total (expt 2 rc-model-total-bits))
    (define rc-shift-bits 5)
    ;; Range coding state
    (define rc.code)
    (define rc.range)
    (define rc.bound)

    (define (make-bit-model n)
      ;; The vector contains integers that represent probabilities.
      (make-vector n (expt 2 (- rc-model-total-bits 1))))

    (define (rc-reset)
      (set! rc.code (bytevector->uint (get-bytevector-n in 5)))
      (set! rc.range #xFFFFFFFF)
      (set! rc.bound 0))

    (define (rc-normalize)
      (when (< rc.range (expt 2 rc-top-bits))
        (set! rc.range (asl rc.range 8))
        (set! rc.code (bitwise-and #xFFFFFFFF
                                   (bitwise-ior (get-u8 in) (asl rc.code 8))))
        (trace "rc-normalize")))

    (define (rc-zero? P i)
      (rc-normalize)
      (let ((Pi (vector-ref P i)))
        (set! rc.bound (* Pi (asr rc.range rc-model-total-bits)))
        (trace ";; " (if (< rc.code rc.bound) 'ZERO 'one)
               ". code: " rc.code " bound: " rc.bound)
        ;; Update probabilities
        (cond ((< rc.code rc.bound)
               (set! rc.range rc.bound)
               (vector-set! P i (fx+ Pi (fxasr (fx- rc-bit-model-total Pi)
                                               rc-shift-bits)))
               #t)
              (else
               (set! rc.range (- rc.range rc.bound))
               (set! rc.code (- rc.code rc.bound))
               (vector-set! P i (fx- Pi (fxasr Pi rc-shift-bits)))
               #f))))

    (define (rc-get-bit P i mi)
      (if (rc-zero? P i)
          (asl mi 1)
          (bitwise-ior (asl mi 1) #b1)))

    (define (rc-get-bit* P i mi)
      (if (rc-zero? P i)
          (values 0 (asl mi 1))
          (values 1 (bitwise-ior (asl mi 1) #b1))))

    (define (rc-bit-tree-decode P i bits)
      (do ((ret 1 (rc-get-bit P (+ i ret) ret))
           (j bits (fx- j 1)))
          ((fxzero? j) (- ret (asl 1 bits)))))

    (define (rc-direct-bit)
      (rc-normalize)
      (set! rc.range (asr rc.range 1))
      (cond ((>= rc.code rc.range)
             (set! rc.code (- rc.code rc.range))
             1)
            (else 0)))

    ;; States
    (define state.literal->literal 0)
    (define state.match->literal->literal 1)
    (define state.repeat->literal->literal 2)
    (define state.short-repeat->literal->literal 3)
    (define state.match->literal 4)
    (define state.repeat->literal 5)
    (define state.short-repeat->literal 6)

    (define state.literal->match 7)
    (define state.literal->long-repeat 8)
    (define state.literal->short-repeat 9)

    (define state.non-literal->match 10)
    (define state.non-literal->repeat 11)

    (define number-of-states (+ state.non-literal->repeat 1))

    (define (literal-state? state)
      (fx<=? state state.short-repeat->literal))

    (define (state+literal state)
      (cond ((fx<=? state state.short-repeat->literal->literal)
             ;; X->literal->literal becomes literal->literal.
             state.literal->literal)
            ((fx<=? state state.literal->short-repeat)
             ;; literal->X becomes X->literal.
             (fx- state (fx- state.literal->short-repeat
                             state.short-repeat->literal)))
            (else
             ;; non-literal->X becomes X->literal.
             (fx- state (fx- state.non-literal->repeat
                             state.repeat->literal)))))

    (define (state+short-repeat state)
      (if (literal-state? state) state.literal->short-repeat state.non-literal->repeat))

    (define (state+long-repeat state)
      (if (literal-state? state) state.literal->long-repeat state.non-literal->repeat))

    (define (state+match state)
      (if (literal-state? state) state.literal->match state.non-literal->match))

    ;; For the encoding of lengths
    (define minimum-match-length 2)
    (define length-bits-low 3)            ;bit-widths
    (define length-bits-middle 3)
    (define length-bits-high 8)
    (define maximum-position-bits 4)      ;maximum for `pb'
    (define length.choice 0)              ;offsets into length coders:
    (define length.choice-2 (+ length.choice 1))
    (define length.low (+ length.choice-2 1))
    (define length.middle (+ length.low (expt 2 (+ maximum-position-bits
                                                   length-bits-low))))
    (define length.high (+ length.middle (expt 2 (+ maximum-position-bits
                                                    length-bits-middle))))
    (define length-coder-size (+ length.high (expt 2 length-bits-high)))

    (define (decode-length decoder position-state)
      ;; Decodes a length. They are encoded (and returned) with
      ;; minimum-match-length subtracted. First there's a prefix code
      ;; that determines a base and then either 3 or 8 bits follow.
      (trace ";; decoding a match length...")
      (cond ((rc-zero? decoder length.choice)
             ;; base = 0. length in [min+base+0,min+base+7] = [2,9].
             (rc-bit-tree-decode decoder
                                 (fx+ length.low (fxasl position-state length-bits-low))
                                 length-bits-low))
            (else
             (cond ((rc-zero? decoder length.choice-2)
                    ;; base = 8. length in [min+base+0,min+base+7] = [10,17].
                    (fx+ (expt 2 length-bits-low)
                         (rc-bit-tree-decode decoder
                                             (fx+ length.middle
                                                  (fxasl position-state length-bits-middle))
                                             length-bits-middle)))
                   (else
                    ;; base = 16. length in [min+base+0,min+base+255] = [18,273].
                    (fx+ (fx+ (expt 2 length-bits-low) (expt 2 length-bits-middle))
                         (rc-bit-tree-decode decoder length.high length-bits-high)))))))

    ;; For the encoding of distances
    (define start-pos-model-index 4)
    (define end-pos-model-index 14)
    (define full-distances (expt 2 (/ end-pos-model-index 2)))
    (define length-to-position-states 4)
    (define distance-bits-pos-slot 6)
    (define distance-bits-alignment 4)        ;the lowest bits

    (define (decode-distance pos-slot-decoders pos-decoders alignment-decoders len)
      ;; Decodes a distance. They are encoded with 1 subtracted (at
      ;; least from the perspective of the sliding-buffer library).
      (define (get-bits decoder base bits distance)
        (let lp ((n 0) (symbol 1) (bits bits) (distance distance))
          (if (fxzero? bits)
              (+ distance 1)
              (let-values (((bit symbol) (rc-get-bit* decoder (+ base symbol) symbol)))
                (if (fxzero? bit)
                    (lp (fx+ n 1) symbol (fx- bits 1) distance)
                    (lp (fx+ n 1) symbol (fx- bits 1)
                        (bitwise-ior distance (asl 1 n))))))))
      (trace ";; decoding a match distance...")
      (let ((pos-slot (rc-bit-tree-decode pos-slot-decoders
                                          ;; Different probabilities are
                                          ;; used for small lengths.
                                          (fxasl (fxmin len (- length-to-position-states 1))
                                                 distance-bits-pos-slot)
                                          distance-bits-pos-slot)))
        (trace "pos-slot: " pos-slot)
        (cond ((fx<? pos-slot start-pos-model-index)
               ;; Distance in [0,3].
               (fx+ pos-slot 1))
              (else
               (let ((bits (fx- (fxasr pos-slot 1) 1))
                     (distance (fxior (fxand pos-slot #b1) #b10)))
                 (cond ((fx<? pos-slot end-pos-model-index)
                        (trace "additional bits")
                        (let* ((dist (fxasl distance bits))
                               (base (- dist pos-slot 1)))
                          (get-bits pos-decoders base bits dist)))
                       (else
                        ;; The distance gets the two highest bits from
                        ;; pos-slot-decoders, the four lowest bits from
                        ;; alignment-decoders, and the rest are "direct"
                        ;; bits that aren't encoded with probabilities.
                        (trace "direct bits")
                        (do ((i (fx- bits distance-bits-alignment) (fx- i 1))
                             (dist distance (bitwise-ior (asl dist 1) (rc-direct-bit))))
                            ((fxzero? i)
                             (trace "alignment bits")
                             (get-bits alignment-decoders 0 distance-bits-alignment
                                       (asl dist distance-bits-alignment)))))))))))

    ;; For the encoding of literals
    (define literal-coder-size #x300)

    (define (decode-literal decoders subcoder match-byte)
      ;; Decode a byte literal.
      (define (get-byte byte)
        (do ((byte byte (rc-get-bit decoders (+ subcoder byte) byte)))
            ((fx>? byte #xff)
             (fxand byte #xff))))
      (if (not match-byte)
          (get-byte 1)
          (let lp ((match-byte match-byte) (byte 1))
            (if (fx>? byte #xff)
                (get-byte byte)
                ;; TODO: describe the three parts of the subcoder
                (let* ((match-byte* (fxasl match-byte 1))
                       (match-bit (fxand match-byte* #x100)))
                  (let-values (((bit byte*)
                                (rc-get-bit* decoders
                                             (+ subcoder #x100 match-bit byte)
                                             byte)))
                    (if (fx=? (fxasr match-bit 8) bit)
                        (lp match-byte* byte*)
                        (get-byte byte*))))))))

    ;; Probability vectors for the range coding
    (let ((match-decoders (make-bit-model (* number-of-states
                                             (expt 2 maximum-position-bits))))
          (rep-decoders (make-bit-model number-of-states))
          (rep-G0-decoders (make-bit-model number-of-states))
          (rep-G1-decoders (make-bit-model number-of-states))
          (rep-G2-decoders (make-bit-model number-of-states))
          (rep-0-long-decoders (make-bit-model (* number-of-states
                                                  (expt 2 maximum-position-bits))))
          (literal-decoders (make-bit-model (asl literal-coder-size #;4 (+ lc lp))))
          (pos-decoders (make-bit-model (- full-distances end-pos-model-index)))
          (pos-slot-decoders (make-bit-model (* length-to-position-states
                                                (expt 2 distance-bits-pos-slot))))
          (alignment-decoders (make-bit-model (expt 2 distance-bits-alignment)))
          (len-decoders (make-bit-model length-coder-size))
          (rep-len-decoders (make-bit-model length-coder-size)))

      (define (get-length/distance position-state state rep0 rep1 rep2 rep3)
        ;; Reads a length and distance code. If the code includes length
        ;; or distance, they are returned. If the corresponding value is
        ;; #f it must be decoded separately. Also returns the new state.
        (if (rc-zero? rep-decoders state)
            (values #f #f len-decoders (state+match state) rep0 rep1 rep2)
            (if (rc-zero? rep-G0-decoders state)
                (if (rc-zero? rep-0-long-decoders
                              (+ (asl state maximum-position-bits) position-state))
                    (values (- 1 minimum-match-length)
                            rep0 rep-len-decoders (state+short-repeat state)
                            rep1 rep2 rep3)
                    (values #f rep0 rep-len-decoders (state+long-repeat state)
                            rep1 rep2 rep3))
                (if (rc-zero? rep-G1-decoders state)
                    (values #f rep1 rep-len-decoders (state+long-repeat state)
                            rep0 rep2 rep3)
                    (if (rc-zero? rep-G2-decoders state)
                        (values #f rep2 rep-len-decoders (state+long-repeat state)
                                rep0 rep1 rep3)
                        (values #f rep3 rep-len-decoders (state+long-repeat state)
                                rep0 rep1 rep2))))))

      (let restart ((dictionary dictionary) (usize usize)
                    (lc lc) (lp lp) (pb pb) (position position)
                    (state state.literal->literal)
                    (rep0 1) (rep1 1) (rep2 1) (rep3 1))
        (rc-reset)
        (let ((position-state-mask (- (asl 1 pb) 1))
              (literal-pos-mask (- (asl 1 lp) 1)))
          ;; Decoder loop
          (let loop ((position position) (chunk-position 0)
                     (state state) (rep0 rep0) (rep1 rep1)
                     (rep2 rep2) (rep3 rep3))
            (cond ((fx>=? chunk-position usize)
                   ;; Return the full state so that it can be restored
                   ;; later. Used by LZMA2 in which a "block" is multiple
                   ;; LZMA chunks.
                   (rc-normalize)
                   (letrec ((restart-lzma
                             (lambda (dictionary usize lc lp pb)
                               (restart dictionary usize lc lp pb
                                        position state rep0 rep1 rep2 rep3))))
                     restart-lzma))
                  (else
                   (let ((position-state (bitwise-and position position-state-mask)))
                     (trace "position = " position " position-state = " position-state)
                     (trace "state: " (list state rep0 rep1 rep2 rep3))
                     (trace "chunk-position = " chunk-position)
                     (cond ((rc-zero? match-decoders
                                      (fx+ position-state
                                           (fxasl state maximum-position-bits)))
                            (trace ";;; LITERAL")
                            (let* ((previous-byte (if (positive? position)
                                                      (sliding-buffer-lookback-u8 dictionary 1)
                                                      0))
                                   (subcoder (* (+ (asl (bitwise-and position literal-pos-mask)
                                                        lc)
                                                   (asr previous-byte (fx- 8 lc)))
                                                literal-coder-size))
                                   (match-byte (and (not (literal-state? state))
                                                    (sliding-buffer-lookback-u8 dictionary
                                                                                rep0)))
                                   (literal (decode-literal literal-decoders
                                                            subcoder match-byte)))
                              (trace "#;literal: " literal)
                              (sliding-buffer-put-u8! dictionary literal))
                            (loop (+ position 1) (fx+ chunk-position 1)
                                  (state+literal state) rep0 rep1 rep2 rep3))
                           (else
                            (trace ";;; MATCH")
                            (let-values (((len distance length-decoder state rep1 rep2 rep3)
                                          (get-length/distance position-state state
                                                               rep0 rep1 rep2 rep3)))
                              (let* ((len (or len (decode-length length-decoder
                                                                 position-state)))
                                     (dist (or distance
                                               (decode-distance pos-slot-decoders
                                                                pos-decoders
                                                                alignment-decoders
                                                                len))))
                                ;; XXX: the distance can encode end-of-stream in
                                ;; old LZMA.
                                (let ((len (+ len minimum-match-length)))
                                  (trace "#;copy: distance=" dist " len=" len)
                                  (sliding-buffer-dup! dictionary dist len)
                                  (loop (+ position len) (fx+ chunk-position len)
                                        state dist rep1 rep2 rep3)))))))))))))))
