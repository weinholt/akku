;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; src/pffi.sls - Portable Foreign Function Interface
;;;
;;;   Copyright (c) 2015-2019  Takashi Kato  <ktakashi@ymail.com>
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

#!r6rs
(library (pffi)
    (export foreign-procedure
	    c-callback
	    free-c-callback
	    open-shared-object
	    ;; TODO should we export these primitives?
	    ;; lookup-shared-object
	    ;; make-c-function
	    ;; make-c-callback

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
	    size-of-uint8_t
	    size-of-uint16_t
	    size-of-uint32_t
	    size-of-uint64_t

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

	    ;; variable
	    define-foreign-variable

	    pointer?
	    bytevector->pointer
	    pointer->bytevector
	    pointer->integer
	    integer->pointer

	    ;; struct
	    define-foreign-struct
	    define-foreign-union
	    )
    (import (pffi procedure)
	    (pffi variable)
	    (pffi pointers)
	    (pffi struct)
	    (only (rnrs) define))

(define size-of-uint8_t  size-of-int8_t)
(define size-of-uint16_t size-of-int16_t)
(define size-of-uint32_t size-of-int32_t)
(define size-of-uint64_t size-of-int64_t)

)
