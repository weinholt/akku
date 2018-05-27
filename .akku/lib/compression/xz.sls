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

;; This library reads Lasse Collin and Igor Pavlov's .xz file format.

;; http://tukaani.org/xz/format.html

(library (compression xz)
  (export make-xz-input-port open-xz-file-input-port #;extract-xz
          is-xz-file?)
  (import (rnrs)
          (compression lzma2)
          (hashing crc)
          (hashing sha-2)               ;for streams
          (struct pack))

  (define-syntax trace
    (syntax-rules ()
      #;
      ((_ . args)
       (begin
         (for-each display (list . args))
         (newline)))
      ((_ . args) (begin 'dummy))))

  (define-crc crc-32)                   ;for headers etc

  (define xz-magic #vu8(#xFD #x37 #x7A #x58 #x5A #x00))

  (define xz-filter-delta #x03)

  (define xz-filter-bcj-x86 #x04)

  (define xz-filter-bcj-powerpc #x05)

  (define xz-filter-bcj-itanium #x06)

  (define xz-filter-bcj-armel #x07)

  (define xz-filter-bcj-armel-thumb #x08)

  (define xz-filter-bcj-sparc #x09)

  (define xz-filter-lzma2 #x21)

  (define (checksum-procedures algorithm)
    ;; Takes a name and returns init, update and finish.
    (case algorithm
      ((crc-32)
       (values crc-32-init
               crc-32-update
               (lambda (checksum)
                 (pack "<L" (crc-32-finish checksum)))))
      ((crc-64/ecma-182)
       (let ()
         (define-crc crc-64/ecma-182)
         (values crc-64/ecma-182-init
                 crc-64/ecma-182-update
                 (lambda (checksum)
                   (pack "<Q" (crc-64/ecma-182-finish checksum))))))
      ((sha-256)
       (values make-sha-256
               (lambda (checksum bv start end)
                 (sha-256-update! checksum bv start end)
                 checksum)
               (lambda (checksum)
                 (sha-256-finish! checksum)
                 (sha-256->bytevector checksum))))
      (else
       ;; Unknown or "none" algorithm.
       (values (lambda () #f)
               (lambda x #f)
               (lambda x #f)))))

  (define (is-xz-file? f)
    (let* ((f (if (input-port? f) f (open-file-input-port f)))
           (pos (port-position f)))
      (set-port-position! f 0)
      (let ((bv (get-bytevector-n f (bytevector-length xz-magic))))
        (set-port-position! f pos)
        (equal? bv xz-magic))))

  (define (get-varint p max-bytes who)
    (let ((max (* (if max-bytes (min 9 max-bytes) 9) 7)))
      (let lp ((ret 0) (shift 0))
        (if (= shift max)
            ret
            (let ((b (get-u8 p)))
              (cond
                ((eof-object? b)
                 (error who "Unexpected end-of-file in variable length integer" p))
                ((zero? b) ret)
                (else
                 (let ((ret* (bitwise-ior ret
                                          (bitwise-arithmetic-shift-left (fxand b #x7f)
                                                                         shift))))
                   (if (fx<=? b #x7f)
                       ret*
                       (lp ret* (+ shift 7)))))))))))

  (define (get-stream-header p)
    (define who 'get-stream-header)
    (define len #vu8(0 4 4 4 8 8 8 16 16 16 32 32 32 64 64 64))
    (define crc '#(none crc-32 #f #f crc-64/ecma-182 #f #f #f #f #f sha-256 #f #f #f #f #f))
    (unless (equal? (get-bytevector-n p (bytevector-length xz-magic)) xz-magic)
      (error who "Expected to find an XZ stream header" p))
    (let-values (((fnull flags) (get-unpack p "CC")))
      (let ((algorithm (fxbit-field flags 0 4))
            (reserved (fxbit-field flags 4 8)))
        (let ((have (crc-32 (pack "CC" fnull flags)))
              (want (get-unpack p "<L")))
          (unless (= have want)
            (error who "Corrupted XZ stream header" p)))
        (unless (zero? reserved)
          (error who "Reserved flags in XZ stream header" p))
        (values (bytevector-u8-ref len algorithm)
                (vector-ref crc algorithm)))))

  (define (get-stream-footer p)
    (define who 'get-stream-footer)
    (define footer-magic #vu8(#x59 #x5A))
    (let-values (((want backward-size fnull flags) (get-unpack p "<LLCC")))
      (unless (equal? footer-magic (get-bytevector-n p 2))
        (error who "Invalid XZ stream footer magic" p))
      (let ((have (crc-32 (pack "<LCC" backward-size fnull flags))))
        (unless (= have want)
          (error who "Corrupted XZ stream footer" p)))
      (values backward-size fnull flags)))

  (define (get-block-header p)
    (define (check-padding hp)
      (let lp ()
        (let ((x (get-u8 hp)))
          (unless (eof-object? x)
            (unless (fxzero? x)
              (error who "Incorrectly parsed XZ block header" p))
            (lp)))))
    (define who 'get-block-header)
    (let ((size (lookahead-u8 p)))
      ;; XXX: size = 0 means index
      (unless (and (integer? size) (positive? size))
        (error who "Bad size field in XZ block header" p))
      (let ((header (get-bytevector-n p (* size 4))))
        (let ((have (crc-32 header))
              (want (get-unpack p "<L")))
          (unless (= have want)
            (error who "Corrupted XZ block header" p)))
        (let ((hp (open-bytevector-input-port header)))
          (let ((flags (get-unpack hp "xC")))
            (let ((filters (fx+ (fxbit-field flags 0 2) 1))
                  (reserved (fxbit-field flags 2 6))
                  (compressed-size? (fxbit-set? flags 6))
                  (uncompressed-size? (fxbit-set? flags 7)))
              (unless (zero? reserved)
                (error who "Reserved flags in XZ block header" p))
              (let* ((csize (and compressed-size? (get-varint hp #f who)))
                     (ucsize (and uncompressed-size? (get-varint hp #f who))))
                (do ((f '()
                        (let* ((id (get-varint hp #f who))
                               (propsize (get-varint hp #f who))
                               (properties (get-bytevector-n hp propsize)))
                          (cons (cons id properties) f)))
                     (i filters (- i 1)))
                    ((zero? i)
                     (check-padding hp)
                     (values f csize ucsize))))))))))

  (define (get-index p)
    (define who 'get-index)
    (let ((pos (port-position p)))
      (unless (zero? (get-u8 p))
        (error 'get-index "Missing index indicator in XZ file" p))
      (do ((records (make-vector (get-varint p #f who) #f))
           (i 0 (+ i 1)))
          ((= i (vector-length records))
           (let ((n (bitwise-and (- (- (port-position p) pos))
                                 #b11)))
             (let ((padding (get-bytevector-n p n)))
               (unless (equal? padding (make-bytevector n 0))
                 (error who "Invalid index padding" padding)))
             ;; TODO: do this without set-port-position!.
             (let ((new-pos (port-position p)))
               (set-port-position! p pos)
               (let ((block (get-bytevector-n p (- new-pos pos))))
                 (let ((have (crc-32 block))
                       (want (get-unpack p "<L")))
                   (unless (= have want)
                     (error who "Corrupted XZ index" p))
                   )))
             records))
        (vector-set! records i
                     (let* ((csize (get-varint p #f who))
                            (usize (get-varint p #f who)))
                       (cons csize usize))))))

  (define (make-xz-input-port in id close-underlying-port?)
    (define (dictsize filters)
      (let ((dict-flags (unpack "C" (cdr (assv xz-filter-lzma2 filters)))))
        (trace "LZMA2 dictionary flags: " dict-flags " from " filters)
        (let ((bits (fxbit-field dict-flags 0 6)))
          ;; XXX: these dictionaries can get pretty large and often
          ;; they are larger than the output itself, which they
          ;; certainly do not have to be.
          (cond ((> bits 40)
                 (error who
                        "The block header specifies an overlarge LZMA2 dictionary"
                        in bits))
                ((= bits 40) #xFFFFFFFF)
                (else
                 (let* ((b (fxior #b10 (fxand bits #b1)))
                        (size (bitwise-arithmetic-shift-left b (+ (fxdiv bits 2)
                                                                  11))))
                   (trace "LZMA2 dictionary size: " size)
                   size))))))
    ;; TODO: the XZ format supports seeking, so port-position and
    ;; set-port-position! could be implemented. TODO: concatenated
    ;; streams. TODO: the conditions here should probably be I/O
    ;; related.
    (define who 'make-xz-input-port)
    (let*-values (((check-length check-algorithm) (get-stream-header in))
                  ((check-init check-update! check-finish!)
                   (checksum-procedures check-algorithm)))
      (trace "XZ check algorithm: " check-algorithm)
      (let ((buffer (make-bytevector (expt 2 15))) ;block buffer
            (buf-r 0)
            (buf-w 0))
        (define checksum (check-init))
        (define block-start #f)
        (define lzma2-dictsize)
        (define lzma2-state #f)

        (define (grow! minimum)
          ;; LZMA chunks can be at most 2MiB when uncompressed.
          (when (fx<? (bytevector-length buffer) minimum)
            (let ((new (make-bytevector (max minimum (fx* (bytevector-length buffer) 2)))))
              (bytevector-copy! buffer 0 new 0 (bytevector-length buffer))
              (set! buffer new))))

        (define (sink bv start count)
          ;; TODO: support for additional filters.
          ;; The sink is only called when there's no data in the buffer.
          (set! checksum (check-update! checksum bv start (+ start count)))
          (grow! count)
          (bytevector-copy! bv start buffer buf-w count)
          (set! buf-w (+ buf-w count)))

        (define (next-block)
          (trace "XZ input port: next-block")
          (cond ((zero? (lookahead-u8 in))
                 (let ((index (get-index in)))
                   ;; TODO: verify the index
                   (trace "XZ index: " index))
                 ;; TODO: the stream footer contains flags that must be checked
                 (get-stream-footer in)
                 #f)
                (else
                 (set! block-start (port-position in))
                 (let-values (((filters csize ucsize) (get-block-header in)))
                   (unless (= (length filters) 1)
                     (error who "TODO: support for more XZ filters" filters))
                   (trace "XZ block compressed size: " csize
                          " uncompressed size: " ucsize)
                   (trace "XZ block filters: " filters)
                   (set! lzma2-dictsize (dictsize filters))
                   #t))))

        (define (next-chunk)
          (trace "XZ input port: next-chunk")
          (set! lzma2-state (lzma2-decode-chunk in sink lzma2-dictsize lzma2-state))
          (when (eof-object? lzma2-state)
            ;; End of LZMA2 block. Read padding and checksum.
            (trace "XZ input port: End of block.")
            (let* ((n (bitwise-and (- (- (port-position in) block-start))
                                   #b11))
                   (padding (get-bytevector-n in n)))
              (set! block-start #f)
              (unless (equal? padding (make-bytevector n 0))
                (error who "Invalid block padding" padding))
              (let ((want (get-bytevector-n in check-length))
                    (have (check-finish! checksum)))
                (trace "LZMA2 block checksum: " want " <-> " have)
                (when (and have (not (equal? want have)))
                  (error who "There has been an LZMA2 block checksum mismatch."
                         want have))))))

        (define (read! bytevector start count)
          ;; Read up to `count' bytes from the source, write them to
          ;; `bytevector' at index `start'. Return the number of bytes
          ;; read (zero means end of file).
          (let loop ()
            (when (zero? buf-w)         ;buffer is empty?
              (trace "XZ input port: needs more data")
              (cond ((not block-start)
                     ;; Start reading a new block if one is available
                     (when (next-block)
                       (next-chunk)))
                    (else
                     (next-chunk)
                     (when (not block-start)
                       ;; This chunk ended the block.
                       (loop))))))
          ;; Return data from the block buffer.
          (let* ((valid (- buf-w buf-r))
                 (returned (min count valid)))
            (bytevector-copy! buffer buf-r bytevector start returned)
            (cond ((= returned valid)
                   ;; The buffer is now empty.
                   (set! buf-r 0)
                   (set! buf-w 0))
                  (else
                   (set! buf-r (+ buf-r returned))))
            returned))

        (define (close)
          (trace "XZ input port closed")
          (when checksum
            ;; Read to the end of the input so that the checksum is verified.
            (let lp ()
              (set! buf-r 0)
              (set! buf-w 0)
              (unless (zero? (read! buffer 0 (bytevector-length buffer)))
                (lp))))
          (set! buffer #f)
          (when close-underlying-port? (close-port in))
          (set! in #f))

        (make-custom-binary-input-port id read! #f #f close))))

  (define (open-xz-file-input-port filename)
    (make-xz-input-port (open-file-input-port filename)
                        (string-append "xz " filename)
                        'close))

  ;; TODO: extract-xz?

  )
