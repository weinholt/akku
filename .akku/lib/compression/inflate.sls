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

;; INFLATE is the decompression of DEFLATE'd data (RFC1951)

;; DEFLATE uses a combination of Huffman coding and LZ77. Huffman
;; coding takes an alphabet and makes it into a binary tree where
;; symbols that are more common have a shorter path from the top of
;; the tree (they are sort of like Morse codes). LZ77 makes it
;; possible to copy parts of the recently decompressed data.

(library (compression inflate)
  (export
    inflate
    make-inflater)
  (import
    (rnrs)
    (compression bitstream)
    (compression huffman)
    (compression sliding-buffer)
    (only (srfi :1 lists) iota))

  (define-syntax trace
    (syntax-rules ()
      #;
      ((_ . args)
       (begin
         (for-each display (list . args))
         (newline)))
      ((_ . args) (begin 'dummy))))

  (define vector->huffman-lookup-table
    (case-lambda
      ((codes)
       (vector->huffman-lookup-table codes 0 (vector-length codes)))
      ((codes start end)
       (do ((i (fx- end 1) (fx- i 1))
            (l '()
               (if (fxzero? (vector-ref codes i))
                   l                       ;zeros don't count
                   (cons (cons (fx- i start) (vector-ref codes i)) l))))
           ((fx<? i start)
            (canonical-codes->lookup-table
             (reconstruct-codes < l)))))))

  (define static-table2
    (vector->huffman-lookup-table
     (list->vector
      (map (lambda (c)
             (cond ((< c 144) 8)
                   ((< c 256) 9)
                   ((< c 280) 7)
                   (else 8)))
           (iota 288)))))

  (define static-table3
    (vector->huffman-lookup-table
     (make-vector 32 5)))

  (define len-extra
    '#(0 0 0 0 0 0 0 0 1 1 1 1 2 2 2 2 3 3 3 3 4 4 4 4 5 5 5 5 0))
  (define len-base
    '#(3 4 5 6 7 8 9 10 11 13 15 17 19 23 27 31 35 43 51 59 67 83 99 115
         131 163 195 227 258))
  (define dist-extra
    '#(0 0 0 0 1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10 11 11 12 12 13 13))
  (define dist-base
    '#(1 2 3 4 5 7 9 13 17 25 33 49 65 97 129 193 257 385 513 769 1025 1537
         2049 3073 4097 6145 8193 12289 16385 24577))

  (define (inflate-cblock buffer br table2 table3)
    (let lp ()
      (let ((code (get-next-code br table2)))
        (cond ((< code 256)         ;literal byte
               (trace "LITERAL:" code)
               (sliding-buffer-put-u8! buffer code)
               (lp))
              ((<= 257 code 285)
               (trace "\nlen code: " code)
               (let* ((len (+ (get-bits br (vector-ref len-extra (- code 257)))
                              (vector-ref len-base (- code 257))))
                      (distcode (get-next-code br table3))
                      (dist (+ (get-bits br (vector-ref dist-extra distcode))
                               (vector-ref dist-base distcode))))
                 (trace "len: " len "  dist: " dist)
                 (trace "COPYING FROM POSITION: " dist  " THIS MUCH: " len)
                 (sliding-buffer-dup! buffer dist len)
                 (lp)))
              ((= 256 code))        ;end of block
              (else
               (error 'inflate "error in compressed data (bad literal/length)"))))))

  (define (inflate-block in buffer br)
    (case (get-bits br 2)               ;block-type
      ((#b00)                           ;non-compressed block
       (align-bit-reader br)            ;seek to a byte boundary
       (let* ((len (get-bits br 16))
              (nlen (get-bits br 16)))
         (trace "non-compressed block: " len)
         (unless (fx=? len (fxand #xffff (fxnot nlen)))
           (error 'inflate "error in non-compressed block length" len nlen))
         (unless (eqv? len (sliding-buffer-read! buffer in len))
           (error 'inflate "premature EOF encountered"))))
      ((#b01)                           ;static Huffman tree
       (trace "block with static Huffman tree")
       (inflate-cblock buffer br static-table2 static-table3))
      ((#b10)                           ;dynamic Huffman tree
       (trace "block with dynamic Huffman tree")
       (let* ((hlit (fx+ 257 (get-bits br 5)))
              (hdist (fx+ 1 (get-bits br 5)))
              (hclen (fx+ 4 (get-bits br 4))))
         (when (or (fx>? hlit 286) (fx>? hclen 19))
           (error 'inflate "bad number of literal/length codes" hlit hclen))
         ;; Up to 19 code lengths are now read...
         (let ((table1
                (do ((order '#(16 17 18 0 8 7 9 6 10 5 11 4 12 3 13 2 14 1 15))
                     (i 0 (fx+ i 1))
                     (codes (make-vector 19 0)))
                    ((fx=? i hclen)
                     ;; The 19 codes represent a canonical
                     ;; Huffman table.
                     (vector->huffman-lookup-table codes))
                  (vector-set! codes (vector-ref order i)
                               (get-bits br 3)))))
           ;; Table 1 is now used to encode the `code-lengths'
           ;; canonical Huffman table.
           (let* ((hlen (fx+ hlit hdist))
                  (code-lengths (make-vector hlen 0)))
             (let lp ((n 0))
               (unless (fx=? n hlen)
                 (let ((blc (get-next-code br table1)))
                   (cond
                     ((fx<? blc 16)     ;literal code
                      (vector-set! code-lengths n blc)
                      (lp (fx+ n 1)))
                     ((fx=? blc 16)     ;copy previous code
                      (do ((rep (fx+ 3 (get-bits br 2)))
                           (prev (vector-ref code-lengths (fx- n 1)))
                           (i 0 (fx+ i 1)))
                          ((fx=? i rep) (lp (fx+ n rep)))
                        (vector-set! code-lengths (fx+ n i) prev)))
                     ((fx=? blc 17)     ;fill with zeros
                      (lp (fx+ n (fx+ 3 (get-bits br 3)))))
                     (else              ;fill with zeros (= blc 18)
                      (lp (fx+ n (fx+ 11 (get-bits br 7)))))))))
             ;; Table 2 is for lengths, literals and the
             ;; end-of-block. Table 3 is for distance codes.
             (let ((table2 (vector->huffman-lookup-table code-lengths 0 hlit))
                   (table3 (vector->huffman-lookup-table code-lengths hlit hlen)))
               (inflate-cblock buffer br table2 table3))))))
      ((#b11)
       (error 'inflate "error in compressed data (bad block type)"))))

  ;; Inflate a complete DEFLATE stream. in and out are binary ports.
  ;; Returns the output's CRC and length.
  (define (inflate in out crc-init crc-update crc-finish)
    (let* ((crc (crc-init))
           (output-len 0)
           (buffer (make-sliding-buffer
                    (lambda (bytevector start count)
                      (put-bytevector out bytevector start count)
                      (set! crc (crc-update crc bytevector start (+ start count)))
                      (set! output-len (+ output-len count)))
                    (* 32 1024)))
           (br (make-bit-reader in)))
      (let lp ()
        (let ((last-block (fx=? (get-bits br 1) 1)))
          (inflate-block in buffer br)
          (cond (last-block
                 (sliding-buffer-drain! buffer)
                 (align-bit-reader br)
                 (values (crc-finish crc)
                         output-len
                         (get-bit-reader-buffer br)))
                (else
                 (lp)))))))

  ;; Returns a procedure that, when called, reads a block from the
  ;; DEFLATE stream. The dictionary is a bytevector that is pre-loaded
  ;; into the sliding buffer, but is not copied to the output.
  (define (make-inflater in sink window-size dictionary)
    (let ((buffer (make-sliding-buffer sink window-size))
          (br (make-bit-reader in)))
      (when dictionary
        (sliding-buffer-init! buffer dictionary))
      (case-lambda
        (()
         (let ((last-block (fx=? (get-bits br 1) 1)))
           (inflate-block in buffer br)
           (sliding-buffer-drain! buffer)
           (if last-block 'done 'more)))
        ((x)
         (case x
           ((get-buffer)
            ;; Inflate needs a little lookahead sometimes, so a byte
            ;; or two might be read that does not belong to the
            ;; deflate stream. This retreives those bytes.
            (align-bit-reader br)
            (get-bit-reader-buffer br))))))))
