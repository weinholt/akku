#!r6rs ;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2010, 2012, 2017, 2018 Göran Weinholt <goran@weinholt.se>
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

;; Procedures that read Tape ARchives

;; (get-header-record binary-input-port)
;;  Reads a tar header and does a checksum verification. Returns the
;;  end of file object when there are no more files in the archive.
;;  The returned object should be inspected with the header-*
;;  procedures, unless you're asking for trouble. After this call you
;;  should use extract-to-port or skip-file, even if it's not a
;;  regular file.

;; (extract-to-port binary-input-port header binary-output-port)
;;  Call this after get-header-record to extract the file to a port.
;;  After this call you can use get-header-record again.

;; (skip-file binary-input-port header)
;;  Works like extract-to-port, but it does not write the file anywhere.

;; (header-name header)
;;  Returns the filename of the file immediately following the header
;;  in the tape archive.

;; (header-typeflag header)
;;  Returns one of these symbols: regular hardlink symlink char block
;;  directory fifo. Only 'regular should contain any extractable data.

;; (header-linkname header)
;;  For files where the typeflag is 'symlink, this indicates where
;;  the symlink points.

;; ...

;; http://www.gnu.org/software/tar/manual/html_section/Formats.html

(library (compression tar)
  (export
    get-header-record
    header-name header-mode header-uid header-gid
    header-size header-mtime header-chksum
    header-typeflag header-linkname
    header-magic header-version header-uname
    header-gname header-devmajor header-devminor
    header-chksum-ok? header-chksum-calculate
    extract-to-port skip-file)
  (import
    (rnrs)
    (only (srfi :13 strings) string-trim-both string-trim-right)
    (only (srfi :19 time) time-monotonic->date make-time))

  (define-syntax trace
    (syntax-rules ()
      #;
      ((_ . args)
       (begin
         (for-each display (list . args))
         (newline)))
      ((_ . args) (begin 'dummy))))

  (define (get-asciiz bv i max)
    (utf8->string
     (call-with-bytevector-output-port
       (lambda (r)
         (let lp ((i i) (max max))
           (unless (zero? max)
             (let ((b (bytevector-u8-ref bv i)))
               (unless (fxzero? b)
                 (put-u8 r b)
                 (lp (fx+ i 1) (fx- max 1))))))))))

  (define (get-octal bv i max)
    (string->number (string-trim-both (get-asciiz bv i max)) 8))

  (define zero-record
    (make-bytevector 512 0))

  (define (zero-record? rec)
    (bytevector=? (rec-bv rec) zero-record))

  (define (premature-eof who tarport)
    (error who "premature end of archive" tarport))

  (define (rec-ref name rec)
    (cond ((bytevector? rec) #f)
          ((assq name rec) => cdr)
          (else #f)))

  (define (rec-bv rec)
    (if (bytevector? rec) rec (cdr (assq 'bv rec))))

;;; Header accessors

  ;; Please use these header accessors and do not rely on type of the
  ;; object returned by get-header-record.

  (define (header-name rec)
    (or (rec-ref 'path rec)
        (get-asciiz (rec-bv rec) 0 100)))

  (define (header-mode rec) (get-octal (rec-bv rec) 100 8))
  (define (header-uid rec) (get-octal (rec-bv rec) 108 8))
  (define (header-gid rec) (get-octal (rec-bv rec) 116 8))
  (define (header-size rec) (get-octal (rec-bv rec) 124 12))
  (define (header-mtime rec)
    (time-monotonic->date
     (make-time 'time-monotonic 0
                (get-octal (rec-bv rec) 136 12))))
  (define (header-chksum rec) (get-octal (rec-bv rec) 148 8))
  (define (header-typeflag rec)
    (let ((t (integer->char (bytevector-u8-ref (rec-bv rec) 156))))
      (case t
        ((#\0 #\nul) 'regular)
        ((#\1) 'hardlink)
        ((#\2) 'symlink)
        ((#\3) 'char)
        ((#\4) 'block)
        ((#\5) 'directory)
        ((#\6) 'fifo)
        ;; Regular file with "high-performance attribute"?
        ((#\7) 'regular)
        (else t))))
  (define (header-linkname rec)
    (or (rec-ref 'linkpath rec)
        (get-asciiz (rec-bv rec) 157 100)))
  (define (header-magic rec) (get-asciiz (rec-bv rec) 257 6))
  (define (header-version rec) (get-octal (rec-bv rec) 263 2))
  (define (header-uname rec) (get-asciiz (rec-bv rec) 265 32))
  (define (header-gname rec) (get-asciiz (rec-bv rec) 297 32))
  (define (header-devmajor rec) (get-octal (rec-bv rec) 329 8))
  (define (header-devminor rec) (get-octal (rec-bv rec) 337 8))

  (define (header-chksum-calculate rec)
    (define (sum bv start end)
      (do ((i start (fx+ i 1))
           (sum 0 (fx+ sum (bytevector-u8-ref bv i))))
          ((fx=? i end) sum)))
    (fx+ (sum (rec-bv rec) 0 148)
         (fx+ 256 #;(sum #vu8(32 32 32 32 32 32 32 32) 0 8)
              (sum (rec-bv rec) 156 512))))

  (define (header-chksum-ok? rec)
    (eqv? (header-chksum (rec-bv rec))
          (header-chksum-calculate (rec-bv rec))))

;;; Tarball reading

  ;; TODO: PAX global header (g)?

  (define (extract-to-bytevector tarport header)
    (call-with-bytevector-output-port
      (lambda (p)
        (extract-to-port tarport header p))))

  (define (parse-pax-header header value->string)
    (define (get/sentinel port sentinel valid?)
      (let lp ((chars '()))
        (let ((b (get-u8 port)))
          (cond ((eof-object? b) (eof-object))
                ((fx=? b (char->integer sentinel))
                 (list->string (reverse chars)))
                ((valid? b)
                 (lp (cons (integer->char b) chars)))
                (else (eof-object))))))
    (call-with-port (open-bytevector-input-port (rec-bv header))
      (lambda (p)
        (let lp ((attr* '()))
          (let* ((len (get/sentinel p #\space
                                    (lambda (b)
                                      (fx<=? (char->integer #\0) b (char->integer #\9)))))
                 (key (get/sentinel p #\= (lambda _ _))))
            (if (eof-object? len)
                attr*
                (let* ((value-len (- (string->number len 10) 1 ;<length> <space>
                                     (string-length len) 1 ;<key> <equals>
                                     (string-length key) 1)) ;<value> <newline>
                       (value (get-bytevector-n p value-len))
                       (newline (get-u8 p)))
                  (unless (and (bytevector? value)
                               (= (bytevector-length value) value-len)
                               (eqv? newline (char->integer #\newline)))
                    (error 'parse-pax-header "Invalid PAX header"
                           value value-len newline))
                  (cons (cons (string->symbol key) (value->string value))
                        (lp attr*)))))))))

  (define (get-header-record tarport)
    (define who 'get-header-record)
    (let ((rec (get-bytevector-n tarport 512)))
      (trace "get-header-record")
      (cond
        ((eof-object? rec) (eof-object))
        ((zero-record? rec) (eof-object))
        ((not (= (bytevector-length rec) 512))
         (premature-eof who tarport))
        ((not (header-chksum-ok? rec))
         (error who "bad tar header checksum" tarport))
        ((and (eqv? (header-typeflag rec) #\L)
              (equal? (header-name rec) "././@LongLink"))
         (trace "reading gnu L")
         (let* ((data (extract-to-bytevector tarport rec))
                (path (string-trim-right (utf8->string data) #\nul)))
           `((path . ,path)
             ,@(get-header-record tarport))))
        ((and (eqv? (header-typeflag rec) #\K)
              (equal? (header-name rec) "././@LongLink"))
         (trace "reading gnu K")
         (let* ((data (extract-to-bytevector tarport rec))
                (linkpath (string-trim-right (utf8->string data) #\nul)))
           `((linkpath . ,linkpath)
             ,@(get-header-record tarport))))
        ((eqv? (header-typeflag rec) #\x)
         (trace "reading pax header")
         (let ((pax-data (extract-to-bytevector tarport rec)))
           ;; (hexdump #f pax-data)
           ;; TODO: the global header might specify something other than utf8
           (append (parse-pax-header pax-data utf8->string)
                   (get-header-record tarport))))
        (else
         `((bv . ,rec))))))

  (define (extract-to-port tarport header destport)
    (define who 'extract-to-port)
    (trace "Extracting " (header-name header)
           " (" (header-size header) ") bytes"
           " from " tarport " to " destport)
    (let*-values (((size) (header-size header))
                  ((blocks trail) (div-and-mod size 512)))
      (trace blocks " blocks and " trail " bytes trailing")
      (do ((buf (make-bytevector 512))
           (blocks blocks (- blocks 1)))
          ((zero? blocks)
           (let ((r (get-bytevector-n! tarport buf 0 512)))
             (trace "read block: " r " (last)")
             (unless (eqv? r 512) (premature-eof who tarport))
             (put-bytevector destport buf 0 trail)))
        (let ((r (get-bytevector-n! tarport buf 0 512)))
          (unless (eqv? r 512) (premature-eof who tarport))
          (trace "read block: " r)
          (put-bytevector destport buf)))))

  (define (skip-file tarport header)
    (define who 'skip-file)
    (trace "Skipping " (header-name header) " from " tarport)
    (let ((blocks (div (+ 511 (header-size header)) 512)))
      (trace blocks " blocks")
      (cond ((eq? 'hardlink (header-typeflag header)))
            ((and (port-has-port-position? tarport)
                  (port-has-set-port-position!? tarport))
             (set-port-position! tarport (+ (port-position tarport)
                                            (* 512 blocks))))
            (else
             (do ((buf (make-bytevector 512))
                  (blocks blocks (- blocks 1)))
                 ((zero? blocks))
               (let ((r (get-bytevector-n! tarport buf 0 512)))
                 (unless (eqv? r 512) (premature-eof who tarport))
                 (trace "read block: " r))))))))
