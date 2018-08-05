#!r6rs ;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2010, 2012, 2017 Göran Weinholt <goran@weinholt.se>
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

;; Read bits from binary input ports.

(library (compression bitstream)
  (export make-bit-reader
          get-bits lookahead-bits align-bit-reader
          get-bit-reader-buffer)
  (import (rnrs))

  ;; (define-record-type bit-reader
  ;;   (fields (immutable port)
  ;;           (mutable buf)
  ;;           (mutable buflen))
  ;;   (protocol
  ;;    (lambda (p)
  ;;      (lambda (port)
  ;;        (p port 0 0)))))

  ;; Records are too slow in Ikarus...
  (define (make-bit-reader port) (vector port 0 0))
  (define (bit-reader-port br) (vector-ref br 0))
  (define (bit-reader-buf br) (vector-ref br 1))
  (define (bit-reader-buflen br) (vector-ref br 2))
  (define (bit-reader-buf-set! br v) (vector-set! br 1 v))
  (define (bit-reader-buflen-set! br v) (vector-set! br 2 v))

  (define (fill! br n)
    (let lp ()
      (let ((buflen (bit-reader-buflen br)))
        (when (fx<? buflen n)           ;read more?
          (bit-reader-buf-set! br (fxior (fxarithmetic-shift-left
                                          (get-u8 (bit-reader-port br))
                                          buflen)
                                         (bit-reader-buf br)))
          (bit-reader-buflen-set! br (fx+ buflen 8))
          (lp)))))

  (define (read! br n)
    (let ((buf (bit-reader-buf br))
          (buflen (bit-reader-buflen br)))
      (bit-reader-buf-set! br (fxarithmetic-shift-right buf n))
      (bit-reader-buflen-set! br (fx- buflen n))
      (fxbit-field buf 0 n)))

  ;; Read n bits from the port
  (define (get-bits br n)
    (fill! br n)
    (read! br n))

  ;; Peek at n bits from the port
  (define (lookahead-bits br n)
    (fill! br n)
    (fxbit-field (bit-reader-buf br) 0 n))

  ;; Not called very often. It is used to discard all bits up until
  ;; the next byte boundary.
  (define (align-bit-reader br)
    (let ((buflen (bit-reader-buflen br)))
      (read! br (fx- buflen (fxand buflen -8)))))

  ;; Return the buffer as a bytevector.
  (define (get-bit-reader-buffer br)
    (let ((buf (bit-reader-buf br))
          (buflen (bit-reader-buflen br)))
      (bit-reader-buf-set! br 0)
      (bit-reader-buflen-set! br 0)
      (if (fxzero? buflen)
          #vu8()
          (let ((bv (make-bytevector (fxdiv (fxand -8 (fx+ 7 buflen)) 8))))
            (bytevector-uint-set! bv 0 buf (endianness big)
                                  (bytevector-length bv))
            bv)))))
