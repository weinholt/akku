;;; -*- mode:scheme; coding: utf-8; -*-
;;;
;;; src/pffi/struct.sls - Foreign structure
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

;; Foreign structure is basically chunk of memory and in this
;; library we represent it as bytevector. (this is because
;; not all implementation have GCable memory allocation.)
;; To pass it to foreign procedures, it needs to be converted
;; to a pointer by bytevector->pointer.

(library (pffi struct)
    (export define-foreign-struct
            define-foreign-union)
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
      (let loop ((clauses clauses) (fs #f) (par #f) (proto #f) (align #f))
        (syntax-case clauses (fields parent protocol)
          (() (list fs par proto align))
          (((fields defs ...) . rest)
           (or (not fs)
               (syntax-violation 'define-foreign-struct
                                 "only one fields clause allowed"
                                 x clauses))
           (loop #'rest (process-fields #'(defs ...)) par proto align))
          (((parent p) . rest)
           (or (not par)
               (syntax-violation 'define-foreign-struct
                                 "only one parent clause allowed"
                                 x clauses))
           (loop #'rest fs (d #'p) proto align))
          (((protocol p) . rest)
           (or (not proto)
               (syntax-violation 'define-foreign-struct
                                 "only one protocol clause allowed"
                                 x clauses))
           (loop #'rest fs par (d #'p) align))
          (((alignment a) . rest)
           (or (not align)
               (syntax-violation 'define-foreign-struct
                                 "only one alignment clause allowed"
                                 x clauses))
           (loop #'rest fs par proto #'a))
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
       (with-syntax (((((type field ref set) ...) parent protocol align)
                      (datum->syntax #'k (process-clauses #'name
                                                          #'(specs ...))))
                     (sizeof (datum->syntax #'k (->sizeof #'name)))
                     ;; To avoid Guile's bug.
                     ;; this isn't needed if macro expander works *properly*
                     ((this-protocol) (generate-temporaries '(this-protocol))))
         (with-syntax ((((types sizeofs) ...)
                        (datum->syntax #'k (->sizeofs #'(type ...)))))
           #'(begin
               (define alignment-check
                 (unless (or (not align) (memv align '(1 2 4 8 16)))
                   (assertion-violation 'define-foreign-struct
                                        "alignment must be  1, 2, 4, 8, or 16"
                                        align)))
               (define sizeof (compute-size parent
                                            (list (cons type sizeofs) ...)
                                            align))
               (define this-protocol protocol)
               (define name
                 (make-foreign-struct-descriptor
                  'name
                  sizeof
                  (struct-alignment (list (cons type sizeofs) ...))
                  (list (cons* 'field types sizeofs) ...)
                  parent
                  (if this-protocol #t #f)
                  ;; type-ref
                  (lambda (o offset)
                    (let ((bv (make-bytevector sizeof)))
                      (bytevector-copy! o offset bv 0 sizeof)
                      bv))
                  ;; type-set!
                  (lambda (o offset v)
                    (bytevector-copy! v 0 o offset sizeof))))

               ;; TODO sub struct
               (define (pred o)
                 (and (bytevector? o)
                      (>= (bytevector-length o) sizeof)))
               ;; TODO handle protocol
               (define ctr (make-constructor name this-protocol))
               (define ref
                 (let ((acc (type->ref type))
                       (offset (compute-offset name 'field align)))
                   (lambda (o) (acc o offset))))
               ...
               (define set
                 (let ((acc (type->set! type))
                       (offset (compute-offset name 'field align)))
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

(define-syntax define-foreign-union
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
      (let loop ((clauses clauses) (fs #f) (proto #f))
        (syntax-case clauses (fields protocol)
          (() (list fs proto))
          (((fields defs ...) . rest)
           (or (not fs)
               (syntax-violation 'define-foreign-struct
                                 "only one fields clause allowed"
                                 x clauses))
           (loop #'rest (process-fields #'(defs ...)) proto))
          (((protocol p) . rest)
           (or (not proto)
               (syntax-violation 'define-foreign-struct
                                 "only one protocol clause allowed"
                                 x clauses))
           (loop #'rest fs (d #'p)))
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
       (with-syntax (((((type field ref set) ...) protocol)
                      (datum->syntax #'k (process-clauses #'name
                                                          #'(specs ...))))
                     (sizeof (datum->syntax #'k (->sizeof #'name)))
                     ;; To avoid Guile's bug.
                     ;; this isn't needed if macro expander works *properly*
                     ((this-protocol) (generate-temporaries '(this-protocol))))
         (with-syntax ((((types sizeofs) ...)
                        (datum->syntax #'k (->sizeofs #'(type ...)))))
           #'(begin
               (define sizeof
                 ;; the same as alignment :)
                 (struct-alignment (list (cons type sizeofs) ...)))
               (define this-protocol protocol)
               (define name
                 (make-foreign-struct-descriptor
                  'name
                  sizeof
                  (struct-alignment (list (cons type sizeofs) ...))
                  ;; only one field so dummy
                  (list (cons* 'dummy 'name sizeof))
                  #f
                  (if this-protocol #t #f)
                  ;; type-ref
                  (lambda (o offset)
                    (let ((bv (make-bytevector sizeof)))
                      (bytevector-copy! o offset bv 0 sizeof)
                      bv))
                  ;; type-set!
                  (lambda (o offset v)
                    (bytevector-copy! v 0 o offset sizeof))))
               ;; sub struct
               (define (pred o)
                 (and (bytevector? o)
                      (>= (bytevector-length o) sizeof)))
               (define ctr
                 (let ()
                   (define fields '(field ...))
                   (define (custom-ctr . field&value)
                     (define f
                       (and (not (null? field&value)) (car field&value)))
                     (define v
                       (and (not (null? field&value))
                            (not (null? (cdr field&value)))
                            (cadr field&value)))
                     (define setters
                       (foreign-struct-descriptor-setters name))
                     (let ((r (make-bytevector sizeof 0)))
                       ;; a bit inefficient...
                       (when (and f v)
                         (do ((i 0 (+ i 1)) (f* fields (cdr f*)))
                             ((or (null? f*) (eq? (car f*) f))
                              (unless (null? f*)
                                (let ((s (list-ref setters i)))
                                  (s r v))))))
                       r))
                   (if this-protocol
                       (this-protocol custom-ctr)
                       (lambda () (make-bytevector sizeof 0)))))
               (define ref
                 (let ((acc (type->ref type)))
                   (lambda (o) (acc o 0))))
               ...
               (define set
                 (let ((acc (type->set! type)))
                   (lambda (o v) (acc o 0 v))))
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

;; ref
;;  http://en.wikipedia.org/wiki/Data_structure_alignment
(define (compute-size parent list-of-sizeofs alignment)
  (define-syntax padding
    (syntax-rules ()
      ((_ o a) ;; offset align
       (if (and alignment (zero? (mod o alignment)))
           0
           (bitwise-and (- o) (- a 1))))))

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
          (let ((m (if alignment (- alignment 1) (- max-size 1))))
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

;; FIXME I'm not sure if I'm doing correctly here but work
;;       (tests are passing...)
(define (compute-offset descriptor field align)
  (define-syntax offset
    (syntax-rules ()
      ((_ o a) ;; offset align
       (let* ((off o)
              (ali (if align align a))
              (n (+ off ali)))
         (bitwise-and (- n 1) (bitwise-not (- ali 1)))))))
  (define-syntax padding
    (syntax-rules ()
      ((_ f o a) ;; offset align
       (let ((field f) (ali a) (off o))
         (if (and align (zero? (mod off align)))
             0
             (bitwise-and (- off) (- a 1)))))))
  (define (compute-next-size size f)
    (let* ((s (cddr f))
           (size (+ size s))
           (a (if (foreign-struct-descriptor? (cadr f))
                  (foreign-struct-descriptor-alignment (cadr f))
                  s))
           (pad (padding (cadr f) size a)))
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
             ;; rough next offset
             (off (offset 0 parent-align)))
    (if (null? fields)
        (assertion-violation 'compute-offset "invalid field name" field)
        (let ((f (car fields)))
          (if (eq? field (car f))
              (if (foreign-struct-descriptor? (cadr f))
                  (offset off (foreign-struct-descriptor-alignment (cadr f)))
                  (let ((next-size (compute-next-size size f)))
                    (- next-size (alignment f))))
              (let ((next-size (compute-next-size size f)))
                ;; rough next offset = current offset + next-size
                (loop (cdr fields) next-size (+ off next-size))))))))


(define-syntax type->set!
  (lambda (x)
    (define (->set! k type)
      (let ((n (syntax->datum type)))
        (if (memq n '(char unsigned-char short unsigned-short int unsigned-int
                      long unsigned-long float double
                      int8_t uint8_t int16_t uint16_t
                      int32_t uint32_t int64_t uint64_t pointer))
            (datum->syntax k
             (string->symbol (string-append (symbol->string n) "-set!")))
            ;; We need syntax context of the given type,
            ;; so use with-syntax here to keep it.
            (with-syntax ((t type))
              #'(foreign-struct-descriptor-type-set! t)))))
    (syntax-case x ()
      ((k type)
       (->set! #'k #'type)))))
(define-syntax type->ref
  (lambda (x)
    (define (->ref k type)
      (let ((n (syntax->datum type)))
        (if (memq n '(char unsigned-char short unsigned-short int unsigned-int
                      long unsigned-long float double
                      int8_t uint8_t int16_t uint16_t
                      int32_t uint32_t int64_t uint64_t pointer))
            (datum->syntax k
             (string->symbol (string-append (symbol->string n) "-ref")))
            ;; We need syntax context of the given type,
            ;; so use with-syntax here to keep it.
            (with-syntax ((t type))
              #'(foreign-struct-descriptor-type-ref t)))))
    (syntax-case x ()
      ((k type) (->ref #'k #'type)))))

;; descriptor for convenience
(define-record-type foreign-struct-descriptor
  (fields name                          ; for debug
          size
          alignment
          fields
          parent
          ;; these will be set after the construction...
          (mutable getters)
          (mutable setters)
          (mutable protocol)
          has-protocol?
          (mutable ctr)
          type-ref
          type-set!)
  (protocol (lambda (n)
              (lambda (nm s a f p p? ref set)
                (n nm s a f p #f #f #f p? #f ref set)))))

(define char-ref           bytevector-s8-ref)
(define unsigned-char-ref  bytevector-u8-ref)
(define short-ref          bytevector-s16-native-ref)
(define unsigned-short-ref bytevector-u16-native-ref)
(define int-ref            bytevector-s32-native-ref)
(define unsigned-int-ref   bytevector-u32-native-ref)
(define long-ref
  (if (= size-of-long 4) bytevector-s32-native-ref bytevector-s64-native-ref))
(define unsigned-long-ref
  (if (= size-of-long 4) bytevector-u32-native-ref bytevector-u64-native-ref))
(define float-ref          bytevector-ieee-single-native-ref)
(define double-ref         bytevector-ieee-double-native-ref)
(define int8_t-ref         bytevector-s8-ref)
(define uint8_t-ref        bytevector-u8-ref)
(define int16_t-ref        bytevector-s16-native-ref)
(define uint16_t-ref       bytevector-u16-native-ref)
(define int32_t-ref        bytevector-s32-native-ref)
(define uint32_t-ref       bytevector-u32-native-ref)
(define int64_t-ref        bytevector-s64-native-ref)
(define uint64_t-ref       bytevector-u64-native-ref)
(define (pointer-ref o offset)
  (integer->pointer
   (if (= size-of-pointer 4)
       (bytevector-u32-ref o offset (native-endianness))
       (bytevector-u64-ref o offset (native-endianness)))))

(define char-set!           bytevector-s8-set!)
(define unsigned-char-set!  bytevector-u8-set!)
(define short-set!          bytevector-s16-native-set!)
(define unsigned-short-set! bytevector-u16-native-set!)
(define int-set!            bytevector-s32-native-set!)
(define unsigned-int-set!   bytevector-u32-native-set!)
(define long-set!
  (if (= size-of-long 4) bytevector-s32-native-set! bytevector-s64-native-set!))
(define unsigned-long-set!
  (if (= size-of-long 4) bytevector-u32-native-set! bytevector-u64-native-set!))
(define float-set!          bytevector-ieee-single-native-set!)
(define double-set!         bytevector-ieee-double-native-set!)
(define int8_t-set!         bytevector-s8-set!)
(define uint8_t-set!        bytevector-u8-set!)
(define int16_t-set!        bytevector-s16-native-set!)
(define uint16_t-set!       bytevector-u16-native-set!)
(define int32_t-set!        bytevector-s32-native-set!)
(define uint32_t-set!       bytevector-u32-native-set!)
(define int64_t-set!        bytevector-s64-native-set!)
(define uint64_t-set!       bytevector-u64-native-set!)
(define (pointer-set! o offset v)
  (let ((bv (uint-list->bytevector
             (list (pointer->integer v))
             (native-endianness)
             size-of-pointer)))
    (bytevector-copy! bv 0 o offset size-of-pointer)))
)
