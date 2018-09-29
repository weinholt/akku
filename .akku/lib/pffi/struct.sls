#!r6rs ;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; src/pffi/struct.sls - Foreign structure
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

;; Foreign structure is basically chunk of memory and in this
;; library we represent it as bytevector. (this is because
;; not all implementation have GCable memory allocation.)
;; To pass it to foreign procedures, it needs to be converted
;; to a pointer by bytevector->pointer.

(library (pffi struct)
    (export define-foreign-struct)
    (import (rnrs)
	    (pffi compat)
	    (only (pffi misc) take drop split-at))

;; use fields, protocol and parent from (rnrs)
;; e.g.
;; (define-foreign-struct foreign-vector
;;   (fields (int size)
;;           (pointer elements))
;;   (protocol
;;     (lambda (p)
;;       (lambda (size)
;;         (p size (bytevector->pointer (make-bytevector size)))))))
;;
;; NB: for now we don't support protocol. it's a bit pain in the ass...
(define-syntax define-foreign-struct
  (lambda (x)
    (define (->ctr&pred name)
      (let ((base (symbol->string (syntax->datum name))))
	(list (string->symbol (string-append "make-" base))
	      (string->symbol (string-append base "?")))))
    (define (process-clauses struct-name clauses)
      (define sname (symbol->string (syntax->datum struct-name)))
      (define d syntax->datum)
      (define (process-fields fields)
	(define (ref name)
	  (string->symbol 
	   (string-append sname "-" (symbol->string (syntax->datum name)))))
	(define (set name)
	  (string->symbol 
	   (string-append sname "-" (symbol->string (syntax->datum name))
			  "-set!")))
	(let loop ((fields fields) (r '()))
	  (syntax-case fields ()
	    (() (reverse r))
	    (((type name) . rest)
	     (loop #'rest 
		   (cons (list (d #'type) (d  #'name)
			       (ref #'name) (set #'name)) r)))
	    (((type name ref) . rest)
	     (loop #'rest 
		   (cons (list (d #'type) (d #'name) (d #'ref)
			       (set #'name)) r)))
	    (((type name ref set) . rest)
	     (loop #'rest
		   (cons (list (d #'type) (d #'name) (d #'ref) (d #'set))
			 r))))))
      (let loop ((clauses clauses) (fs #f) (par #f) (proto #f))
	(syntax-case clauses (fields parent protocol)
	  (() (list fs par proto))
	  (((fields defs ...) . rest)
	   (or (not fs)
	       (syntax-violation 'define-foreign-struct 
				 "only one fields clause allowed" 
				 x clauses))
	   (loop #'rest (process-fields #'(defs ...)) par proto))
	  (((parent p) . rest)
	   (or (not par)
	       (syntax-violation 'define-foreign-struct 
				 "only one parent clause allowed" 
				 x clauses))
	   (loop #'rest fs (d #'p) proto))
	  (((protocol p) . rest)
	   (or (not proto)
	       (syntax-violation 'define-foreign-struct 
				 "only one protocol clause allowed" 
				 x clauses))
	   (loop #'rest fs par (d #'p)))
	  (_ (syntax-violation 'define-foreign-struct
			       "invalid define-foreign-struct" x clauses)))))

    (define (->sizeof type)
      (string->symbol (string-append "size-of-"
				     (symbol->string (syntax->datum type)))))
    (define (->sizeofs types)
      (let loop ((types types) (r '()))
	(syntax-case types ()
	  (() (reverse r))
	  ((a . d) (loop #'d (cons (list (syntax->datum #'a) 
					 (->sizeof #'a)) r))))))

    (syntax-case x ()
      ((k (name ctr pred) specs ...)
       (and (identifier? #'name) (identifier? #'ctr) (identifier? #'pred))
       ;; collect fields, parent and protocol
       (with-syntax (((((type field ref set) ...) parent protocol)
		      (datum->syntax #'k (process-clauses #'name 
							  #'(specs ...))))
		     (sizeof (datum->syntax #'k (->sizeof #'name)))
		     ;; To avoid Guile's bug.
		     ;; this isn't needed if macro expander works *properly*
		     ((this-protocol) (generate-temporaries '(this-protocol))))
	 (with-syntax ((((types sizeofs) ...) 
			(datum->syntax #'k (->sizeofs #'(type ...)))))
	   #'(begin 
	       (define sizeof (compute-size parent 
					    (list (cons type sizeofs) ...)))
	       (define this-protocol protocol) 
	       (define name 
		 (let ()
		   (hashtable-set! *struct-set!* 'name
				   (lambda (o offset v)
				     (bytevector-copy! v 0 o offset sizeof)))
		   (hashtable-set! *struct-ref* 'name
				   (lambda (o offset)
				     (let ((bv (make-bytevector sizeof)))
				       (bytevector-copy! o offset bv 0 sizeof)
				       bv)))
		   (make-foreign-struct-descriptor 
		    'name
		    sizeof
		    (struct-alignment (list (cons type sizeofs) ...))
		    (list (cons* 'field types sizeofs) ...)
		    parent
		    (if this-protocol #t #f))))
	       ;; TODO sub struct
	       (define (pred o) 
		 (and (bytevector? o) 
		      (>= (bytevector-length o) sizeof)))
	       ;; TODO handle protocol
	       (define ctr (make-constructor name this-protocol))
	       (define ref
		 (let ((acc (type->ref 'type))
		       (offset (compute-offset name 'field)))
		   (lambda (o) (acc o offset))))
	       ...
	       (define set
		 (let ((acc (type->set! 'type))
		       (offset (compute-offset name 'field)))
		   (lambda (o v) (acc o offset v))))
	       ...
	       ;; ugly...
	       (define dummy 
		 (begin
		   (foreign-struct-descriptor-getters-set! name (list ref ...))
		   (foreign-struct-descriptor-setters-set! name (list set ...))
		   (foreign-struct-descriptor-ctr-set! name ctr)
		   (foreign-struct-descriptor-protocol-set! name 
		    (or this-protocol (default-protocol name)))))
	       )
	   )))
      ((k name specs ...)
       (identifier? #'name)
       (with-syntax (((ctr pred) (datum->syntax #'k (->ctr&pred #'name))))
	 #'(k (name ctr pred) specs ...))))))


(define (total-field-count desc)
  (let loop ((desc desc) (r 0))
    (if desc
	(loop (foreign-struct-descriptor-parent desc)
	      (+ (length (foreign-struct-descriptor-fields desc)) r))
	r)))

(define (make-struct desc field-values)
  (define (set-parent-fields desc bv field-values)
    (define (->ordered-paretns desc)
      (let loop ((p (foreign-struct-descriptor-parent desc)) (r '()))
	(if p
	    (loop (foreign-struct-descriptor-parent p) (cons p r))
	    (reverse r))))
    (let loop ((parents (->ordered-paretns desc))
	       (field-values field-values))
      (if (null? parents)
	  field-values
	  (let* ((setters (foreign-struct-descriptor-setters (car parents)))
		 (len (length setters)))
	    (for-each (lambda (set arg) (set bv arg)) 
		      setters (take field-values len))
	    (loop (cdr parents) (drop field-values len))))))
  (let ((setters (foreign-struct-descriptor-setters desc))
	(bv (make-bytevector (foreign-struct-descriptor-size desc))))
    (let ((field-values (set-parent-fields desc bv field-values)))
      (for-each (lambda (set arg) (set bv arg)) setters field-values)
      bv)))

(define (make-simple-conser protocol desc argc)
  (protocol
   (lambda field-values
     (if (= (length field-values) argc)
	 (make-struct desc field-values)
	 (assertion-violation "struct constructor"
			      "wrong number of arguments"
			      field-values)))))

(define (make-nested-conser protocol odesc argc)
  (protocol
   ((let loop ((desc odesc))
      (cond ((foreign-struct-descriptor-parent desc)
	     => (lambda (parent)
		  (lambda extra-field-values
		    (lambda protocol-args
		      (lambda this-field-values
			(apply ((foreign-struct-descriptor-protocol parent)
				(apply (loop parent)
				       (append this-field-values
					       extra-field-values)))
			       protocol-args))))))
	    (else
	     (lambda extra-field-values
	       (lambda this-field-values
		 (let ((field-values (append this-field-values
					     extra-field-values)))
		   (if (= (length field-values) argc)
		       (make-struct odesc field-values)
		       (assertion-violation "struct constructor"
					    "wrong number of arguments"
					    field-values)))))))))))

(define (default-protocol desc)
  (let ((parent (foreign-struct-descriptor-parent desc)))
    (if parent
	(let ((parent-field-count (total-field-count parent)))
	  (lambda (p)
	    (lambda field-values
	      (let-values (((parent-field-values this-field-values)
			    (split-at field-values parent-field-count)))
		(let ((n (apply p parent-field-values)))
		  (apply n this-field-values))))))
	(lambda (p)
	  (lambda field-values
	    (apply p field-values))))))

;; TODO implement it properly...
(define (make-constructor desc protocol)
  (let ((parent? (foreign-struct-descriptor-parent desc))
	(protocol (or protocol (default-protocol desc))))
    (if parent?
	;; check parent protocol
	(begin
	  (when (and (foreign-struct-descriptor-has-protocol? parent?)
		     (not (foreign-struct-descriptor-has-protocol? desc)))
	    (assertion-violation 'make-constructor 
				 "parent has custom protocol" desc))
	  (make-nested-conser protocol desc
			      (total-field-count desc)))
	(make-simple-conser protocol desc 
			    (length (foreign-struct-descriptor-fields desc))))))

(define (struct-alignment lis)
  (apply max (map (lambda (l)
		    (if (foreign-struct-descriptor? (car l))
			(foreign-struct-descriptor-alignment (car l))
			(cdr l))) lis)))
;; TODO this is just packed size so it's not correct
;; ref
;;  http://en.wikipedia.org/wiki/Data_structure_alignment
(define (compute-size parent list-of-sizeofs) 
  (define-syntax padding
    (syntax-rules ()
      ((_ o a) ;; offset align
       (bitwise-and (- o) (- a 1)))))

  (let ((parent-size (if parent (foreign-struct-descriptor-size parent) 0))
	(align (if parent (foreign-struct-descriptor-alignment parent) 0)))
    ;; assume primitive types have the same alignment as its size
    ;; (according to the Wikipedia page, linix may have different value
    ;;  for double but never seen it)
    ;; struct manages own alignment
    (let loop ((sizes list-of-sizeofs) 
	       (max-size align) 
	       (size parent-size))
      (if (null? sizes)
	  ;; fixup
	  (let ((m (- max-size 1)))
	    (bitwise-and (+ size m) (bitwise-not m)))
	  (let ((s (car sizes)))
	    (if (foreign-struct-descriptor? (car s))
		(let* ((sa (foreign-struct-descriptor-alignment (car s)))
		       (ss (cdr s))
		       (size (+ size ss))
		       (pad (padding size sa)))
		  (loop (cdr sizes)
			(if (> sa max-size) sa max-size)
			(+ size pad)))
		;; primitive
		(let* ((ps (cdr s))
		       (size (+ size ps))
		       (pad (padding size ps)))
		  (loop (cdr sizes)
			(if (> ps max-size) ps max-size)
			(+ size pad)))))))))

(define (compute-offset descriptor field)
  (define-syntax offset
    (syntax-rules ()
      ((_ o a) ;; offset align
       (bitwise-and (- (+ o a) 1) (bitwise-not (- a 1))))))
  (define-syntax padding
    (syntax-rules ()
      ((_ o a) ;; offset align
       (bitwise-and (- o) (- a 1)))))
  (define (compute-next-size size f)
    (let* ((s (cddr f))
	   (size (+ size s))
	   (a (if (foreign-struct-descriptor? (cadr f))
		  (foreign-struct-descriptor-alignment (cadr f))
		  s))
	   (pad (padding size a)))
      (+ size pad)))
  (define parent-align (if (foreign-struct-descriptor-parent descriptor)
			   (foreign-struct-descriptor-alignment
			    (foreign-struct-descriptor-parent descriptor))
			   0))
  (define (alignment field)
    (if (foreign-struct-descriptor? (cadr field))
	(foreign-struct-descriptor-alignment (cadr field))
	(cddr field)))
  (let loop ((fields (foreign-struct-descriptor-fields descriptor))
	     ;; well...
	     (size (if (foreign-struct-descriptor-parent descriptor)
		       (foreign-struct-descriptor-size
			(foreign-struct-descriptor-parent descriptor))
		       0))
	     (off (offset 0 parent-align)))
    (if (null? fields)
	(assertion-violation 'compute-offset "invalid field name" field)
	(let ((f (car fields)))
	  (if (eq? field (car f))
	      (if (foreign-struct-descriptor? (cadr f))
		  (offset (if (zero? off) off (+ off 1))
			  (foreign-struct-descriptor-alignment (cadr f)))
		  (let ((next-size (compute-next-size size f)))
		    (- next-size (alignment f))))
	      (let ((next-size (compute-next-size size f)))
		(loop (cdr fields) next-size
		      (- next-size (alignment f)))))))))
	  

(define (type->set! type)
  (cond ((hashtable-ref *struct-set!* type #f))
	(else (assertion-violation 'type->set! "unknown type" type))))
(define (type->ref type)
  (cond ((hashtable-ref *struct-ref* type #f))
	(else (assertion-violation 'type->set! "unknown type" type))))

;; descriptor for convenience
(define-record-type foreign-struct-descriptor
  (fields name				; for debug
	  size
	  alignment
	  fields
	  parent
	  ;; these will be set after the construction...
	  (mutable getters)
	  (mutable setters)
	  (mutable protocol)
	  has-protocol?
	  (mutable ctr))
  (protocol (lambda (n)
	      (lambda (nm s a f p p?)
		(n nm s a f p #f #f #f p? #f)))))

(define *struct-ref*
  (let ((ht (make-eq-hashtable)))
    (hashtable-set! ht 'char bytevector-s8-ref)
    (hashtable-set! ht 'unsigned-char bytevector-u8-ref)
    (hashtable-set! ht 'short bytevector-s16-native-ref)
    (hashtable-set! ht 'unsigned-short bytevector-u16-native-ref)
    (hashtable-set! ht 'int bytevector-s32-native-ref)
    (hashtable-set! ht 'unsigned-int bytevector-u32-native-ref)
    (hashtable-set! ht 'long (if (= size-of-long 4)
				 bytevector-s32-native-ref
				 bytevector-s64-native-ref))
    (hashtable-set! ht 'unsigned-long (if (= size-of-long 4)
					  bytevector-u32-native-ref
					  bytevector-u64-native-ref))
    (hashtable-set! ht 'float bytevector-ieee-single-native-ref)
    (hashtable-set! ht 'double bytevector-ieee-double-native-ref)
    (hashtable-set! ht 'int8_t bytevector-s8-ref)
    (hashtable-set! ht 'uint8_t bytevector-u8-ref)
    (hashtable-set! ht 'int16_t bytevector-s16-native-ref)
    (hashtable-set! ht 'uint16_t bytevector-u16-native-ref)
    (hashtable-set! ht 'int32_t bytevector-s32-native-ref)
    (hashtable-set! ht 'uint32_t bytevector-u32-native-ref)
    (hashtable-set! ht 'int64_t bytevector-s64-native-ref)
    (hashtable-set! ht 'uint64_t bytevector-u64-native-ref)
    (hashtable-set! ht 'pointer
		    (lambda (o offset)
		      (integer->pointer
		       (if (= size-of-pointer 4)
			   (bytevector-u32-ref o offset (native-endianness))
			   (bytevector-u64-ref o offset (native-endianness))))))
    ht))

(define *struct-set!*
  (let ((ht (make-eq-hashtable)))
    (hashtable-set! ht 'char bytevector-s8-set!)
    (hashtable-set! ht 'unsigned-char bytevector-u8-set!)
    (hashtable-set! ht 'short bytevector-s16-native-set!)
    (hashtable-set! ht 'unsigned-short bytevector-u16-native-set!)
    (hashtable-set! ht 'int bytevector-s32-native-set!)
    (hashtable-set! ht 'unsigned-int bytevector-u32-native-set!)
    (hashtable-set! ht 'long (if (= size-of-long 4)
				 bytevector-s32-native-set!
				 bytevector-s64-native-set!))
    (hashtable-set! ht 'unsigned-long (if (= size-of-long 4)
					  bytevector-u32-native-set!
					  bytevector-u64-native-set!))
    (hashtable-set! ht 'float bytevector-ieee-single-native-set!)
    (hashtable-set! ht 'double bytevector-ieee-double-native-set!)
    (hashtable-set! ht 'int8_t bytevector-s8-set!)
    (hashtable-set! ht 'uint8_t bytevector-u8-set!)
    (hashtable-set! ht 'int16_t bytevector-s16-native-set!)
    (hashtable-set! ht 'uint16_t bytevector-u16-native-set!)
    (hashtable-set! ht 'int32_t bytevector-s32-native-set!)
    (hashtable-set! ht 'uint32_t bytevector-u32-native-set!)
    (hashtable-set! ht 'int64_t bytevector-s64-native-set!)
    (hashtable-set! ht 'uint64_t bytevector-u64-native-set!)
    (hashtable-set! ht 'pointer
		    (lambda (o offset v)
		      (let ((bv (uint-list->bytevector 
				 (list (pointer->integer v))
				 (native-endianness)
				 size-of-pointer)))
			(bytevector-copy! bv 0 o offset size-of-pointer))))
    ht))

    )
