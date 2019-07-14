;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; src/pffi/compat.guile.sls - Compatible layer for Guile
;;;  
;;;   Copyright (c) 2015  Takashi Kato  <ktakashi@ymail.com>
;;;   
;;;   Redistribution and use in source and binary forms, with or without
;;;   modification, are permitted provided that the following conditions
;;;   are met:
;;;   
;;;   1. Redistributions of source code must retain the above copyright
;;;      notice, this list of conditions and the following disclaimer.
;;;  
;;;   2. Redistributions in binary form must reproduce the above copyright
;;;      notice, this list of conditions and the following disclaimer in the
;;;      documentation and/or other materials provided with the distribution.
;;;  
;;;   THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
;;;   "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
;;;   LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
;;;   A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
;;;   OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
;;;   SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
;;;   TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;;;   PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;;;   LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
;;;  

;; this file provides compatible layer for (pffi procedure)
;; if implementations can't make this layer, then make
;; pffi/procedure.$name.sls file so that (pffi) library can
;; look it up.

#!r6rs
(library (pffi compat)
    (export open-shared-object
	    lookup-shared-object
	    make-c-function
	    make-c-callback
	    free-c-callback

	    ;; primitive types
	    char  unsigned-char
	    short unsigned-short
	    int   unsigned-int
	    long  unsigned-long
	    float double
	    int8_t  uint8_t
	    int16_t uint16_t
	    int32_t uint32_t
	    int64_t uint64_t
	    pointer callback
	    void

	    ;; pointer ref
	    pointer-ref-c-uint8
	    pointer-ref-c-int8
	    pointer-ref-c-uint16
	    pointer-ref-c-int16
	    pointer-ref-c-uint32
	    pointer-ref-c-int32
	    pointer-ref-c-uint64
	    pointer-ref-c-int64
	    pointer-ref-c-unsigned-char
	    pointer-ref-c-char
	    pointer-ref-c-unsigned-short
	    pointer-ref-c-short
	    pointer-ref-c-unsigned-int
	    pointer-ref-c-int
	    pointer-ref-c-unsigned-long
	    pointer-ref-c-long
	    pointer-ref-c-float
	    pointer-ref-c-double
	    pointer-ref-c-pointer

	    ;; pointer set
	    pointer-set-c-uint8!
	    pointer-set-c-int8!
	    pointer-set-c-uint16!
	    pointer-set-c-int16!
	    pointer-set-c-uint32!
	    pointer-set-c-int32!
	    pointer-set-c-uint64!
	    pointer-set-c-int64!
	    pointer-set-c-unsigned-char!
	    pointer-set-c-char!
	    pointer-set-c-unsigned-short!
	    pointer-set-c-short!
	    pointer-set-c-unsigned-int!
	    pointer-set-c-int!
	    pointer-set-c-unsigned-long!
	    pointer-set-c-long!
	    pointer-set-c-float!
	    pointer-set-c-double!
	    pointer-set-c-pointer!
	    
	    ;; sizeof
	    size-of-char
	    size-of-short
	    size-of-int
	    size-of-long
	    size-of-float
	    size-of-double
	    size-of-pointer
	    size-of-int8_t
	    size-of-int16_t
	    size-of-int32_t
	    size-of-int64_t

	    pointer?
	    bytevector->pointer
	    pointer->bytevector
	    (rename (pointer-address pointer->integer)
		    (make-pointer integer->pointer))
	    )
    (import (rnrs)
	    (only (guile) dynamic-link dynamic-pointer)
	    (system foreign)
	    (only (srfi :13) string-index-right))

(define-syntax callback
  (syntax-rules ()
    ((_ ignore ...) pointer)))
(define char           int8)
(define unsigned-char  uint8)
;;(define short          int16)
;;(define unsigned-short uint16)
(define pointer        '*)
(define int8_t         int8)
(define uint8_t        uint8)
(define int16_t        int16)
(define uint16_t       uint16)
(define int32_t        int32)
(define uint32_t       uint32)
(define int64_t        int64)
(define uint64_t       uint64)

(define (open-shared-object path)
  (let* ((index (string-index-right path #\.))
	 (file (if index
		   (substring path 0 index)
		   path)))
    (dynamic-link file)))
(define (lookup-shared-object lib name)
  (dynamic-pointer name lib))

(define (free-c-callback proc) #t) ;; for now.

(define (make-c-function lib ret name arg-types)
  (pointer->procedure ret (lookup-shared-object lib (symbol->string name))
		      arg-types))

(define (make-c-callback ret args proc)
  (procedure->pointer ret proc args))

(define-syntax define-deref
  (lambda (x)
    (define (gen-name t)
      (let ((s (symbol->string (syntax->datum t))))
	(list (string->symbol (string-append "size-of-" s))
	      (string->symbol (string-append "pointer-ref-c-" s))
	      (string->symbol (string-append "pointer-set-c-" s "!")))))
    (syntax-case x ()
      ((k type bv-ref bv-set!)
       (with-syntax (((size ref set!) (datum->syntax #'k (gen-name #'type))))
	 #'(begin
	     (define (ref ptr offset)
	       (let ((bv (pointer->bytevector ptr (+ size offset))))
		 (bv-ref bv offset (native-endianness))))
	     (define (set! ptr offset value)
	       (let ((bv (pointer->bytevector ptr (+ size offset))))
		 (bv-set! bv offset value (native-endianness))))))))))
(define (bytevector-u8-ref/endian bv index endian)
  (bytevector-u8-ref bv index))
(define (bytevector-u8-set/endian! bv index v endian)
  (bytevector-u8-set! bv index v))
(define (bytevector-s8-ref/endian bv index endian)
  (bytevector-s8-ref bv index))
(define (bytevector-s8-set/endian! bv index v endian)
  (bytevector-s8-set! bv index v))

;; kinda tricky
(define (bytevector-long-ref bv index endian)
  (if (= size-of-long 4)
      (bytevector-s32-ref bv index endian)
      (bytevector-s64-ref bv index endian)))
(define (bytevector-long-set! bv index v endian)
  (if (= size-of-long 4)
      (bytevector-s32-set! bv index v endian)
      (bytevector-s64-set! bv index v endian)))
(define (bytevector-ulong-ref bv index endian)
  (if (= size-of-long 4)
      (bytevector-u32-ref bv index endian)
      (bytevector-u64-ref bv index endian)))
(define (bytevector-ulong-set! bv index v endian)
  (if (= size-of-long 4)
      (bytevector-u32-set! bv index v endian)
      (bytevector-u64-set! bv index v endian)))


(define-deref char bytevector-s8-ref/endian bytevector-s8-set/endian!)
(define-deref unsigned-char bytevector-u8-ref/endian bytevector-u8-set/endian!)
(define-deref short bytevector-s16-ref bytevector-s16-set!)
(define-deref unsigned-short bytevector-u16-ref bytevector-u16-set!)
(define-deref int bytevector-s32-ref bytevector-s32-set!)
(define-deref unsigned-int bytevector-u32-ref bytevector-u32-set!)
(define-deref long bytevector-long-ref bytevector-long-set!)
(define-deref unsigned-long bytevector-ulong-ref bytevector-ulong-set!)
(define-deref float bytevector-ieee-single-ref bytevector-ieee-single-set!)
(define-deref double bytevector-ieee-double-ref bytevector-ieee-double-set!)
(define-deref int8 bytevector-s8-ref/endian bytevector-s8-set/endian!)
(define-deref uint8 bytevector-u8-ref/endian bytevector-u8-set/endian!)
(define-deref int16 bytevector-s16-ref bytevector-s16-set!)
(define-deref uint16 bytevector-u16-ref bytevector-u16-set!)
(define-deref int32 bytevector-s32-ref bytevector-s32-set!)
(define-deref uint32 bytevector-u32-ref bytevector-u32-set!)
(define-deref int64 bytevector-s64-ref bytevector-s64-set!)
(define-deref uint64 bytevector-u64-ref bytevector-u64-set!)

(define (bytevector-pointer-ref bv index endian)
  (make-pointer
   (if (= size-of-pointer 4)
       (bytevector-u32-ref bv index endian)
       (bytevector-u64-ref bv index endian))))
(define (bytevector-pointer-set! bv index v endian)
  (let ((i (pointer-address v)))
    (if (= size-of-pointer 4)
	(bytevector-u32-set! bv index i endian)
	(bytevector-u64-set! bv index i endian))))
(define-deref pointer bytevector-pointer-ref bytevector-pointer-set!)


(define-syntax define-sizeof
  (lambda (x)
    (define (gen-name t)
      (let ((s (symbol->string (syntax->datum t))))
	(string->symbol (string-append "size-of-" s))))
    (syntax-case x ()
      ((k type)
       (with-syntax ((name (datum->syntax #'k (gen-name #'type))))
	 #'(define name (sizeof type)))))))

(define-sizeof char)
(define-sizeof short)
(define-sizeof int)
(define-sizeof long)
(define-sizeof pointer)
(define-sizeof float)
(define-sizeof double)
(define-sizeof int8_t)
(define-sizeof int16_t)
(define-sizeof int32_t)
(define-sizeof int64_t)
;; for define-deref
(define size-of-unsigned-char   size-of-char)
(define size-of-unsigned-short  size-of-short)
(define size-of-unsigned-int    size-of-int)
(define size-of-unsigned-long   size-of-long)
(define size-of-int8            size-of-int8_t)
(define size-of-uint8           size-of-int8_t)
(define size-of-int16           size-of-int16_t)
(define size-of-uint16          size-of-int16_t)
(define size-of-int32           size-of-int32_t)
(define size-of-uint32          size-of-int32_t)
(define size-of-int64           size-of-int64_t)
(define size-of-uint64          size-of-int64_t)

)
