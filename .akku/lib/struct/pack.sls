#!r6rs ;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2008, 2009, 2010, 2011, 2012, 2017 Göran Weinholt <goran@weinholt.se>

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

;; Syntax for packing and unpacking C structs using bytevectors

;;; Syntax

;; This syntax is similar to Python's struct module or Perl's
;; pack/unpack functions.

;; The syntax of the format string is as follows:

;; x: padding; c: s8; C: u8; s: s16; S: u16; l: s32; L: u32; q: s64;
;; Q: u64; f: ieee-single; d: ieee-double; ! or >: big-endian (network
;; byte order); <: little-endian; =: native-endian. u: disable natural
;; alignment. a: enable natural alignment. Whitespace is ignored.
;; Format characters can be prefixed with a decimal number, which
;; repeats the format character. Padding is done with zeros.

;; Fields are by default aligned to their natural alignment! This
;; means that NUL bytes are inserted as necessary to have a field's
;; index be aligned to its size.

;; If a format string parameter is not a string datum at expand time,
;; then the syntax expands to a procedure call.

;; (unpack fmt bytevector)
;; (unpack fmt bytevector offset)
;;   Returns as many values as there are fields in the format string.
;;   The values are fetched from the bytevector, possibly at an offset
;;   from the start of it. E.g., if the format string is "C", this
;;   translates to a single bytevector-u8-ref.

;; (pack fmt values ...)
;;   Returns a bytevector containing the values encoded as per the
;;   format string.

;; (pack! fmt bytevector offset values ...)
;;   The same as pack, except it modifies the given bytevector and
;;   returns no values.

;; (get-unpack binary-input-port fmt)
;;   Reads (format-size fmt) bytes from the input port and unpacks
;;   them according to the format string. Returns the same values as
;;   unpack.

;; (format-size fmt)
;;   Returns how many bytes the fields in the format string would use
;;   if packed together, including any padding.

;;; Examples

;; (pack "!xd" 3.14)
;; => #vu8(0 0 0 0 0 0 0 0 64 9 30 184 81 235 133 31)

;; (unpack "!xd" (pack "!xd" 3.14))
;; => 3.14

;; (format-size "!xd")
;; => 16

;; (format-size "!uxd")
;; => 9

;;; Example expansions

;; (get-unpack port "<u5S 3L SS")
;; ->
;; (let ((bv (get-bytevector-n port 26))
;;       (off 0))
;;   (values (bytevector-u16-ref bv 0 (endianness little))
;;           (bytevector-u16-ref bv 2 (endianness little))
;;           (bytevector-u16-ref bv 4 (endianness little))
;;           (bytevector-u16-ref bv 6 (endianness little))
;;           (bytevector-u16-ref bv 8 (endianness little))
;;           (bytevector-u32-ref bv 10 (endianness little))
;;           (bytevector-u32-ref bv 14 (endianness little))
;;           (bytevector-u32-ref bv 18 (endianness little))
;;           (bytevector-u16-ref bv 22 (endianness little))
;;           (bytevector-u16-ref bv 24 (endianness little))))

;; (get-unpack port "4xCCxCC7x")
;; ->
;; (let ((bv (get-bytevector-n port 16))
;;       (off 0))
;;   (values (bytevector-u8-ref bv 4) (bytevector-u8-ref bv 5)
;;           (bytevector-u8-ref bv 7) (bytevector-u8-ref bv 8)))

;; (pack "!SS" (question-qtype x) (question-qclass x))
;; ->
;; (let ((bv (make-bytevector 4)))
;;   (pack! "!SS" bv 0 (question-qtype x) (question-qclass x))
;;   bv)
;; ->
;; (let ((bv (make-bytevector 4)))
;;   (let ((bv bv) (off 0))
;;     (bytevector-u16-set! bv 0 (question-qtype x) (endianness big))
;;     (bytevector-u16-set! bv 2 (question-qclass x) (endianness big))
;;     (values))
;;   bv)

;; Non-constant offsets also work, but the offsets have to be computed
;; at runtime, and it becomes the compiler's job to optimize:

;; (unpack "!uSS" bv end)
;; ->
;; (let ((bv bv) (off end))
;;   (values (bytevector-u16-ref bv off (endianness big))
;;           (bytevector-u16-ref bv (+ off 2) (endianness big))))

;; (pack! "cL" bv offset -1 42)
;; ->
;; (let ((bv bv) (off offset))
;;   (bytevector-s8-set! bv off -1)
;;   (bytevector-zero! bv (+ off 1) (bitwise-and (+ (+ off 1) 3) -4))
;;   (bytevector-u32-native-set! bv (bitwise-and (+ (+ off 1) 3) -4) 42)
;;   (values))

;; The calls to bytevector-zero! are there to put in zeros where the
;; padding is. The bitwise-and is to align the indices.

(library (struct pack)
  (export format-size pack pack! unpack get-unpack put-pack)
  (import (rnrs)
          (for (prefix (struct pack private) aux:)
               expand run))

  (define-syntax unpack*
    (lambda (x)
      (define (type c)
        (case c
          ((#\c) (values 's8 #'bytevector-s8-ref 1)) ;special cases
          ((#\C) (values 'u8 #'bytevector-u8-ref 1))
          ((#\s) (values #'bytevector-s16-ref
                         #'bytevector-s16-native-ref 2))
          ((#\S) (values #'bytevector-u16-ref
                         #'bytevector-u16-native-ref 2))
          ((#\l) (values #'bytevector-s32-ref
                         #'bytevector-s32-native-ref 4))
          ((#\L) (values #'bytevector-u32-ref
                         #'bytevector-u32-native-ref 4))
          ((#\q) (values #'bytevector-s64-ref
                         #'bytevector-s64-native-ref 8))
          ((#\Q) (values #'bytevector-u64-ref
                         #'bytevector-u64-native-ref 8))
          ((#\f) (values #'bytevector-ieee-single-ref
                         #'bytevector-ieee-single-native-ref 4))
          ((#\d) (values #'bytevector-ieee-double-ref
                         #'bytevector-ieee-double-native-ref 8))
          (else (syntax-violation
                 'unpack "Bad character in format string" x c))))
      (syntax-case x ()
        ((_ fmt bytevector)
         #'(unpack* fmt bytevector 0))
        ((_ fmt bytevector offset)
         (with-syntax (((refs ...)
                        (let ((fmt (syntax->datum #'fmt)))
                          (let lp ((i 0)
                                   ;; If the offset is an integer,
                                   ;; then the offsets for all fields
                                   ;; can be computed directly.
                                   ;; Otherwise, code is generated to
                                   ;; compute the offsets.
                                   (o (if (integer? (syntax->datum #'offset))
                                          (syntax->datum #'offset)
                                          #'off))
                                   (rep #f)
                                   (endian #f)
                                   (align #t)
                                   (refs '()))
                            (cond ((= i (string-length fmt))
                                   (reverse refs))
                                  ((char-whitespace? (string-ref fmt i))
                                   (lp (+ i 1) o rep endian align refs))
                                  (else
                                   (case (string-ref fmt i)
                                     ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                                      (lp (+ i 1) o
                                          (+ (- (char->integer (string-ref fmt i))
                                                (char->integer #\0))
                                             (* (if rep rep 0) 10))
                                          endian align refs))
                                     ((#\=)
                                      (lp (+ i 1) o #f #f align refs))
                                     ((#\<)
                                      (lp (+ i 1) o #f #'(endianness little) align refs))
                                     ((#\> #\!)
                                      (lp (+ i 1) o #f #'(endianness big) align refs))
                                     ((#\x)
                                      (lp (+ i 1) (aux:add o (or rep 1)) #f endian align refs))
                                     ((#\a)
                                      (lp (+ i 1) o #f endian #t refs))
                                     ((#\u)
                                      (lp (+ i 1) o #f endian #f refs))
                                     (else
                                      (let-values (((ref nref n) (type (string-ref fmt i))))
                                        (let ((o (if align (aux:roundb o n) o))
                                              (rep (or rep 1)))
                                          (lp (+ i 1) (aux:add o (* n rep)) #f
                                              endian align
                                              (let lp* ((o o) (rep rep) (refs refs))
                                                (if (zero? rep) refs
                                                    (lp* (aux:add o n) (- rep 1)
                                                         (with-syntax ((foff o))
                                                           (cons (cond ((eq? ref 's8)
                                                                        #`(bytevector-s8-ref bv foff))
                                                                       ((eq? ref 'u8)
                                                                        #`(bytevector-u8-ref bv foff))
                                                                       (endian
                                                                        #`(#,ref bv foff #,endian))
                                                                       ((not align)
                                                                        #`(#,ref bv foff (native-endianness)))
                                                                       (else
                                                                        #`(#,nref bv foff)))
                                                                 refs))))))))))))))))
           #'(let ((bv bytevector)
                   (off offset))
               (values refs ...)))))))

  (define unpack**
    (case-lambda
      ((fmt bv offset)
       (define (type c)
         (case c
           ((#\c) (values 's8 bytevector-s8-ref 1)) ;special cases
           ((#\C) (values 'u8 bytevector-u8-ref 1))
           ((#\s) (values bytevector-s16-ref
                          bytevector-s16-native-ref 2))
           ((#\S) (values bytevector-u16-ref
                          bytevector-u16-native-ref 2))
           ((#\l) (values bytevector-s32-ref
                          bytevector-s32-native-ref 4))
           ((#\L) (values bytevector-u32-ref
                          bytevector-u32-native-ref 4))
           ((#\q) (values bytevector-s64-ref
                          bytevector-s64-native-ref 8))
           ((#\Q) (values bytevector-u64-ref
                          bytevector-u64-native-ref 8))
           ((#\f) (values bytevector-ieee-single-ref
                          bytevector-ieee-single-native-ref 4))
           ((#\d) (values bytevector-ieee-double-ref
                          bytevector-ieee-double-native-ref 8))
           (else (error 'unpack "Bad character in format string" fmt c))))
       (let lp ((i 0)
                (o offset)
                (rep #f)
                (endian #f)
                (align #t)
                (refs '()))
         (cond ((= i (string-length fmt))
                (apply values (reverse refs)))
               ((char-whitespace? (string-ref fmt i))
                (lp (+ i 1) o rep endian align refs))
               (else
                (case (string-ref fmt i)
                  ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                   (lp (+ i 1) o
                       (+ (- (char->integer (string-ref fmt i))
                             (char->integer #\0))
                          (* (if rep rep 0) 10))
                       endian align refs))
                  ((#\=)
                   (lp (+ i 1) o #f #f align refs))
                  ((#\<)
                   (lp (+ i 1) o #f (endianness little) align refs))
                  ((#\> #\!)
                   (lp (+ i 1) o #f (endianness big) align refs))
                  ((#\x)
                   (lp (+ i 1) (+ o (or rep 1)) #f endian align refs))
                  ((#\a)
                   (lp (+ i 1) o #f endian #t refs))
                  ((#\u)
                   (lp (+ i 1) o #f endian #f refs))
                  (else
                   (let-values (((ref nref n) (type (string-ref fmt i))))
                     (let ((o (if align (aux:roundb o n) o))
                           (rep (or rep 1)))
                       (lp (+ i 1) (+ o (* n rep)) #f
                           endian align
                           (let lp* ((o o) (rep rep) (refs refs))
                             (if (zero? rep) refs
                                 (lp* (+ o n) (- rep 1)
                                      (cons (cond ((eq? ref 's8)
                                                   (bytevector-s8-ref bv o))
                                                  ((eq? ref 'u8)
                                                   (bytevector-u8-ref bv o))
                                                  (endian
                                                   (ref bv o endian))
                                                  ((not align)
                                                   (ref bv o (native-endianness)))
                                                  (else
                                                   (nref bv o)))
                                            refs)))))))))))))
      ((fmt bv)
       (unpack** fmt bv 0))))

  ;; Use the unpack* expander if possible, otherwise use the unpack**
  ;; function.
  (define-syntax unpack
    (make-variable-transformer
     (lambda (x)
       (syntax-case x ()
         ((_ fmt bytevector) #'(unpack fmt bytevector 0))
         ((_ fmt bytevector offset)
          (string? (syntax->datum #'fmt))
          #'(unpack* fmt bytevector offset))
         ((_ . rest) #'(unpack** . rest))
         (_ #'unpack**)))))

  (define-syntax format-size
    (make-variable-transformer
     (lambda (x)
       (syntax-case x ()
         ((_ fmt)
          (string? (syntax->datum #'fmt))
          (aux:format-size (syntax->datum #'fmt)))
         ((_ fmt)
          #'(aux:format-size fmt))))))

  (define (get-unpack** port fmt)
    (unpack fmt (get-bytevector-n port (format-size fmt))))

  (define-syntax get-unpack
    (make-variable-transformer
     (lambda (x)
       (syntax-case x ()
         ((_ port fmt)
          #'(unpack fmt (get-bytevector-n port (format-size fmt))))
         (var
          (identifier? #'var)
          #'get-unpack**)))))

  (define (bytevector-zero! bv start end)
    (do ((i start (+ i 1)))
        ((= i end))
      (bytevector-u8-set! bv i 0)))

  (define-syntax pack!*
    (lambda (x)
      (define (type c)
        (case c
          ((#\c) (values 's8 #'bytevector-s8-set! 1)) ;special cases
          ((#\C) (values 'u8 #'bytevector-u8-set! 1))
          ((#\s) (values #'bytevector-s16-set!
                         #'bytevector-s16-native-set! 2))
          ((#\S) (values #'bytevector-u16-set!
                         #'bytevector-u16-native-set! 2))
          ((#\l) (values #'bytevector-s32-set!
                         #'bytevector-s32-native-set! 4))
          ((#\L) (values #'bytevector-u32-set!
                         #'bytevector-u32-native-set! 4))
          ((#\q) (values #'bytevector-s64-set!
                         #'bytevector-s64-native-set! 8))
          ((#\Q) (values #'bytevector-u64-set!
                         #'bytevector-u64-native-set! 8))
          ((#\f) (values #'bytevector-ieee-single-set!
                         #'bytevector-ieee-single-native-set! 4))
          ((#\d) (values #'bytevector-ieee-double-set!
                         #'bytevector-ieee-double-native-set! 8))
          (else (syntax-violation
                 'unpack "Bad character in format string" x c))))
      (define (drop vals n fmt)
        (cond ((zero? n) vals)
              ((null? (syntax->datum vals))
               (syntax-violation #f "Too few values for the format" fmt))
              (else
               (with-syntax (((val1 vals ...) vals))
                 (drop #'(vals ...) (- n 1) fmt)))))
      (define (zeroers start end)
        ;; Return code which sets the bytes between start and end to
        ;; zero.
        (cond ((and (integer? start) (integer? end))
               (do ((i start (+ i 1))
                    (setters '() (cons #`(bytevector-u8-set! bv #,i 0)
                                       setters)))
                   ((= i end) setters)))
              ((eq? start end) '())
              (else
               (list #`(bytevector-zero! bv #,start #,end)))))
      (syntax-case x ()
        ((_ fmt* bytevector offset vals ...)
         (with-syntax (((setters ...)
                        (let ((fmt (syntax->datum #'fmt*)))
                          (let lp ((i 0)
                                   ;; If the offset is an integer,
                                   ;; then the offsets for all fields
                                   ;; can be computed directly.
                                   ;; Otherwise, code is generated to
                                   ;; compute the offsets.
                                   (o (if (integer? (syntax->datum #'offset))
                                          (syntax->datum #'offset)
                                          #'off))
                                   (rep #f)
                                   (endian #f)
                                   (align #t)
                                   (setters '())
                                   (vals #'(vals ...)))
                            (cond ((= i (string-length fmt))
                                   (unless (null? (syntax->datum vals))
                                     (syntax-violation #f "Too many values for the format" #'fmt*))
                                   (reverse setters))
                                  ((char-whitespace? (string-ref fmt i))
                                   (lp (+ i 1) o rep endian align setters vals))
                                  (else
                                   (case (string-ref fmt i)
                                     ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                                      (lp (+ i 1) o
                                          (+ (- (char->integer (string-ref fmt i))
                                                (char->integer #\0))
                                             (* (if rep rep 0) 10))
                                          endian align setters vals))
                                     ((#\=) (lp (+ i 1) o #f #f align setters vals))
                                     ((#\<) (lp (+ i 1) o #f #'(endianness little) align setters vals))
                                     ((#\> #\!) (lp (+ i 1) o #f #'(endianness big) align setters vals))
                                     ((#\x)
                                      (lp (+ i 1) (aux:add o (or rep 1)) #f endian align
                                          (append (zeroers o (aux:add o (or rep 1))) setters) vals))
                                     ((#\a)
                                      (lp (+ i 1) o #f endian #t setters vals))
                                     ((#\u)
                                      (lp (+ i 1) o #f endian #f setters vals))
                                     (else
                                      (let*-values (((set nset n) (type (string-ref fmt i)))
                                                    ((rep) (or rep 1))
                                                    ((startoff) (if align (aux:roundb o n) o)))
                                        (lp (+ i 1) (aux:add startoff (* n rep)) #f
                                            endian align
                                            (let lp* ((o* startoff) (rep rep) (vals vals)
                                                      (setters (append (zeroers o startoff) setters)))
                                              (cond ((zero? rep)
                                                     setters)
                                                    (else
                                                     (when (null? (syntax->datum vals))
                                                       (syntax-violation #f "Too few values for the format" #'fmt*))
                                                     (with-syntax ((foff o*)
                                                                   ((val1 vals ...) vals))
                                                       (lp* (aux:add o* n) (- rep 1) #'(vals ...)
                                                            (cons (cond ((eq? set 's8)
                                                                         #`(bytevector-s8-set! bv foff val1))
                                                                        ((eq? set 'u8)
                                                                         #`(bytevector-u8-set! bv foff val1))
                                                                        (endian
                                                                         #`(#,set bv foff val1 #,endian))
                                                                        ((not align)
                                                                         #`(#,set bv foff val1 (native-endianness)))
                                                                        (else
                                                                         #`(#,nset bv foff val1)))
                                                                  setters))))))
                                            (drop vals rep #'fmt*)))))))))))
           #'(let ((bv bytevector)
                   (off offset))
               setters ...
               (values)))))))

  (define (pack!** fmt bv offset . vals)
    (define (type c)
      (case c
        ((#\c) (values 's8 1))          ;special cases
        ((#\C) (values 'u8 1))
        ((#\s) (values bytevector-s16-set! 2))
        ((#\S) (values bytevector-u16-set! 2))
        ((#\l) (values bytevector-s32-set! 4))
        ((#\L) (values bytevector-u32-set! 4))
        ((#\q) (values bytevector-s64-set! 8))
        ((#\Q) (values bytevector-u64-set! 8))
        ((#\f) (values bytevector-ieee-single-set! 4))
        ((#\d) (values bytevector-ieee-double-set! 8))
        (else (error 'pack! "Bad character in format string" fmt c))))
    (define (zero! i n)
      (do ((i i (+ i 1))
           (m (+ i n)))
          ((= i m))
        (bytevector-u8-set! bv i 0)))
    (let lp ((i 0)
             (o offset)
             (rep #f)
             (endian (native-endianness))
             (align #t)
             (vals vals))
      (cond ((= i (string-length fmt))
             (unless (null? vals)
               (error 'pack! "Too many values for the format" fmt)))
            ((char-whitespace? (string-ref fmt i))
             (lp (+ i 1) o rep endian align vals))
            (else
             (case (string-ref fmt i)
               ((#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9)
                (lp (+ i 1) o
                    (+ (- (char->integer (string-ref fmt i))
                          (char->integer #\0))
                       (* (if rep rep 0) 10))
                    endian align vals))
               ((#\=) (lp (+ i 1) o #f (native-endianness) align vals))
               ((#\<) (lp (+ i 1) o #f (endianness little) align vals))
               ((#\> #\!) (lp (+ i 1) o #f (endianness big) align vals))
               ((#\x)
                (zero! o (or rep 1))
                (lp (+ i 1) (+ o (or rep 1)) #f endian align vals))
               ((#\a)
                (lp (+ i 1) o #f endian #t vals))
               ((#\u)
                (lp (+ i 1) o #f endian #f vals))
               (else
                (let*-values (((set n) (type (string-ref fmt i)))
                              ((o*) (if align (aux:roundb o n) o)))
                  (zero! o (- o* o))
                  (do ((rep (or rep 1) (- rep 1))
                       (o o* (+ o n))
                       (vals vals (cdr vals)))
                      ((zero? rep)
                       (lp (+ i 1) (+ o (* n rep)) #f endian align vals))
                    (when (null? vals)
                      (error 'pack! "Too few values for the format" fmt))
                    (cond ((eq? set 's8)
                           (bytevector-s8-set! bv o (car vals)))
                          ((eq? set 'u8)
                           (bytevector-u8-set! bv o (car vals)))
                          (else
                           (set bv o (car vals) endian)))))))))))

  (define-syntax pack!
    (make-variable-transformer
     (lambda (x)
       (syntax-case x ()
         ((_ fmt bv offset vals ...)
          (string? (syntax->datum #'fmt))
          #'(pack!* fmt bv offset vals ...))
         ((_ . rest) #'(pack!** . rest))
         (var
          (identifier? #'var)
          #'pack!**)))))

  (define (pack** fmt . values)
    (let ((bv (make-bytevector (format-size fmt))))
      (apply pack! fmt bv 0 values)
      bv))

  (define-syntax pack
    (make-variable-transformer
     (lambda (x)
       (syntax-case x ()
         ((_ fmt vals ...)
          #'(let ((bv (make-bytevector (format-size fmt))))
              (pack! fmt bv 0 vals ...)
              bv))
         (var
          (identifier? #'var)
          #'pack**)))))

  (define (put-pack** port fmt . vals)
    (put-bytevector port (apply pack fmt vals)))

  (define-syntax put-pack
    (make-variable-transformer
     (lambda (x)
       (syntax-case x ()
         ((_ port fmt vals ...)
          #'(put-bytevector port (pack fmt vals ...)))
         ((_ . rest)
          #'(put-pack** . rest))
         (var
          (identifier? #'var)
          #'put-pack**))))))
