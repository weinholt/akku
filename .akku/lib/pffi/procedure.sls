;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; src/pffi/procedure.sls - FFI Procedure
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

#!r6rs
(library (pffi procedure)
    (export foreign-procedure
	    c-callback
	    free-c-callback
	    open-shared-object
	    lookup-shared-object

	    ;; primitive types
	    ;; maybe these should be exported from
	    ;; different library
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
	    )
    (import (rnrs)
	    (pffi compat))

(define-syntax foreign-procedure
  (syntax-rules ()
    ((_ lib ret name (args ...))
     (make-c-function lib ret 'name (list args ...)))))

(define-syntax c-callback
  (lambda (x)
    (syntax-case x (lambda)
      ((k ret ((type var) ...) (lambda (formals ...) body1 body ...))
       (or (for-all bound-identifier=? #'(var ...) #'(formals ...))
	   (syntax-violation 'c-callback "invalid declaration" '#'x))
       #'(k ret (type ...) (lambda (var ...) body1 body ...)))
      ((_ ret (args ...) proc)
       #'(make-c-callback ret (list args ...) proc)))))

)
