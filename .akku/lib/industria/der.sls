#!r6rs ;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2009, 2010, 2012 Göran Weinholt <goran@weinholt.se>

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

;; Distinguished Encoding Rules (DER)

;; http://www.itu.int/ITU-T/studygroups/com17/languages/X.690-0207.pdf

;; TODO: output

(library (industria der)
  (export decode translate
          data-type
          data-start-index
          data-length
          data-value
          
          bit-string-length bit-string-unused bit-string->bytevector
          bit-string->integer bit-string-bit-set?)
  (import (rnrs)
          (only (srfi :1 lists) take)
          (srfi :19 time)
          (srfi :26 cut)
          (industria bytevectors))

  (define-syntax print
    (syntax-rules ()
      #;
      ((_ . args)
       (begin
         (for-each display (list . args))
         (newline)))
      ((_ . args) (values))))

;;; Bit strings
  
  (define-record-type bit-string
    (fields length unused >bytevector))

  (define (bit-string->integer bs)
    (bitwise-arithmetic-shift-right
     (bytevector-uint-ref (bit-string->bytevector bs) 0 (endianness big)
                          (bit-string-length bs))
     (bit-string-unused bs)))

  (define (bit-string-bit-set? bs i)
    (let ((byte (fxarithmetic-shift-right i 3))
          (index (fxand i 7)))
      (and (fx<? byte (bit-string-length bs))
           (fxbit-set? (bytevector-u8-ref (bit-string->bytevector bs) byte)
                       (fx- 7 index)))))
  
;;; The code that follows reads DER encoded data from bytevectors and
;;; turns it into a parse tree.

  (define (get-type bv start end)
    (let* ((t (bytevector-u8-ref bv start))
           (class (vector-ref '#(universal application context private)
                              (fxbit-field t 6 8)))
           (constructed (fxbit-set? t 5))
           (number (fxbit-field t 0 5)))
      (if (fx=? number #b11111)
          (let lp ((i (+ start 1)) (number 0))
            (unless (< i end)
              (error 'get-type "went over a cliff"))
            (let* ((v (bytevector-u8-ref bv i))
                   (number (bitwise-ior (bitwise-arithmetic-shift-left number 7)
                                        (fxand v #x7f))))
              (if (fxbit-set? v 7)
                  (lp (+ i 1) number)
                  (values constructed class number (+ i 1)))))
          (values constructed class number (+ start 1)))))

  (define (get-length bv start end)
    (let ((b1 (bytevector-u8-ref bv start)))
      (cond ((fx=? b1 #b10000000)       ;indefinite form
             (error 'get-length "indefinite length encountered in DER coding"))
            ((fxbit-set? b1 7)          ;long form
             (let ((extra (fxbit-field b1 0 7)))
               (unless (< (+ start extra 1) end)
                 (error 'get-length "went over a cliff"))
               (let ((len (bytevector-uint-ref bv (+ start 1) (endianness big)
                                               extra)))
                 (when (< len 128)
                   (error 'get-length "unnecessarily long length in DER coding" len))
                 (values len (+ start 1 extra)))))
            (else                       ;short form
             (values b1 (+ start 1))))))

  (define (get-boolean bv start length)
    (unless (= length 1)
      (error 'get-boolean "boolean of illegal length"))
    (let ((v (bytevector-u8-ref bv start)))
      (cond ((= v #xff) #t)
            ((= v #x00) #f)
            (else
             (error 'get-boolean
                    "illegal boolean encoding in DER mode" v)))))

  (define (get-integer bv start length)
    (bytevector-uint-ref bv start (endianness big) length))

  (define (get-T61String bv start length)
    ;; T.61 TeletexString. The world's most complicated character
    ;; encoding ever designed by committee, but according to RFC5280
    ;; everyone treats this as ISO-8859-1. Support is optional...
    (let ((ret (make-bytevector length)))
      (bytevector-copy! bv start ret 0 length)
      (list->string (map integer->char (bytevector->u8-list ret)))))

  (define (get-IA5String bv start length)
    ;; T.50: The International Reference Alphabet No. 5. This used to
    ;; be weird, but they changed it in 1992 so it can be interpreted
    ;; precisely like US-ASCII.
    (let ((ret (make-bytevector length)))
      (bytevector-copy! bv start ret 0 length)
      (let ((bytes (bytevector->u8-list ret)))
        (unless (for-all (cut < <> #x80) bytes)
          (error 'get-IA5String "invalid character"))
        (list->string (map integer->char bytes)))))

  (define (get-PrintableString bv start length)
    (let ((ret (make-bytevector length)))
      (bytevector-copy! bv start ret 0 length)
      (let ((chars (map integer->char (bytevector->u8-list ret))))
        (unless (for-all (lambda (c)
                           (or (char<=? #\0 c #\9)
                               (char<=? #\a c #\z)
                               (char<=? #\A c #\Z)
                               (memq c '(#\space #\' #\( #\) #\+ #\,
                                         #\- #\. #\/ #\: #\= #\?
                                         ;; not really permitted:
                                         #\*))))
                         chars)
          (error 'get-PrintableString "invalid character"))
        (list->string chars))))

  (define (get-UTF8String bv start length)
    (let ((ret (make-bytevector length)))
      (bytevector-copy! bv start ret 0 length)
      (utf8->string ret)))

  (define (get-UniversalString bv start length)
    (let ((ret (make-bytevector length)))
      (bytevector-copy! bv start ret 0 length)
      (utf16->string ret (endianness big)))) ;FIXME: verify this

  (define (get-GraphicString bv start length)
    ;; TODO: this is not really UTF-8
    (utf8->string (subbytevector bv start (+ start length))))

  (define (get-VisibleString bv start length)
    ;; TODO: this is not really UTF-8
    (utf8->string (subbytevector bv start (+ start length))))

  (define (get-GeneralString bv start length)
    ;; TODO: this is not really UTF-8
    (utf8->string (subbytevector bv start (+ start length))))

  (define (get-octet-string bv start length)
    (let ((ret (make-bytevector length)))
      (bytevector-copy! bv start ret 0 length)
      ret))

  (define (get-bit-string bv start length)
    ;; This is an arbitrary string of bits, which is not exactly the
    ;; same thing as a string of octets or an integer. The length
    ;; doesn't have to divide eight. The first byte specifies how many
    ;; bits at the end are unused.
    (if (= length 1)
        0
        (let ((unused (bytevector-u8-ref bv start)))
          (when (> unused 7)
            (error 'get-bit-string "too many trailing bits in bit-string"))
          (make-bit-string (- length 1)
                           unused
                           (get-octet-string bv (+ start 1) (- length 1))))))

  (define (get-UTCTime bv start length)
    ;; YYMMDDHHMMSSZ
    (string->date (string-append (get-IA5String bv start length)
                                 "+0000")
                  "~y~m~d~H~M~SZ~z"))

  (define (get-GeneralizedTime bv start length)
    ;; YYYYMMDDhhmmssZ  YYYYMMDDhhmmss.s+Z
    (define (parse time)
      ;; FIXME: check for invalid encodings
      (define (time-fraction time)
        ;; Returns the fraction. SRFI-19 has nanosecond precision, so at
        ;; most 9 decimals are useful.
        (substring time (string-length "YYYYMMDDhhmmss.")
                   (min (string-length "YYYYMMDDhhmmss.123456789")
                        (- (string-length time) 1))))
      (define (fraction->nanoseconds frac)
        (* (string->number frac 10)
           (expt 10 (- 9 (string-length frac)))))
      (let ((nsec                         ;nanoseconds
             (if (= (string-length time)
                    (string-length "YYYYMMDDhhmmssZ"))
                 0
                 (fraction->nanoseconds (time-fraction time))))
            ;; date without fractional seconds
            (d (string->date (string-append (substring time 0 (string-length
                                                               "YYYYMMDDhhmmss"))
                                            "Z+0000")
                             "~Y~m~d~H~M~SZ~z")))
        ;; add the nanoseconds
        (time-utc->date (add-duration (date->time-utc d)
                                      (make-time 'time-duration nsec 0))
                        0)))
    (parse (get-IA5String bv start length)))
  
  (define (get-oid bv start length relative?)
    (define (get i)
      (if (= i (+ start length))
          '()
          (let lp ((i i) (number 0))
            (unless (< i (+ start length))
              (error 'decode "went over a cliff"))
            (let* ((v (bytevector-u8-ref bv i))
                   (number (bitwise-ior (bitwise-arithmetic-shift-left number 7)
                                        (fxand v #x7f))))
              (if (fxbit-set? v 7)
                  (lp (+ i 1) number)
                  (cons number (get (+ i 1))))))))
    (define (solve-oid p)
      ;; Solve for X,Y in p=(X*40)+Y, X=0..2. For X in (0,1): Y=0..39.
      (cond ((< p 40) (values 0 p))
            ((< p 80) (values 1 (- p 40)))
            (else     (values 2 (- p 80)))))
    (let ((subids (get start)))
      (cond ((null? subids)
             (error 'decode "empty OID"))
            (relative?
             subids)
            (else
             (let-values (((x y) (solve-oid (car subids))))
               (unless (= (car subids) (+ (* 40 x) y))
                 (error 'decode "unable to decode first byte in OID"
                        (car subids)))
               (cons x (cons y (cdr subids))))))))

  (define (get-sequence/set bv start length)
    (let ((end (+ start length)))
      (let lp ((start start)
               (ret '()))
        (if (= start end)
            (reverse ret)
            (let-values (((start* value) (get-value bv start end)))
              (lp start* (cons value ret)))))))

  ;; These are the names you need to use in your type definitions.
  (define universal-types
    `#((reserved #f #f)                 ;end of contents marker, not used in DER
       (boolean ,get-boolean #f)
       (integer ,get-integer #f)
       (bit-string ,get-bit-string #f)
       (octet-string ,get-octet-string #f)
       (null ,(lambda (bv start length) #f) #f)
       (object-identifier ,(lambda (bv start length) (get-oid bv start length #f)) #f)
       (object-descriptor #f #f)
       (external #f #f)
       (real #f #f)
       (enumerated #f #f)
       (embedded-pdv #f #f)
       (utf8-string ,get-UTF8String #f)
       (relative-oid ,(lambda (bv start length) (get-oid bv start length #t)) #f)
       (reserved #f #f)
       (reserved #f #f)
       (sequence #f ,get-sequence/set)
       (set #f ,get-sequence/set)
       (numeric-string #f #f)
       (printable-string ,get-PrintableString #f)
       (t61-string ,get-T61String #f)
       (videotex-string #f #f)
       (ia5-string ,get-IA5String #f)
       (utc-time ,get-UTCTime #f)
       (generalized-time ,get-GeneralizedTime #f)
       (graphic-string ,get-GraphicString #f)
       (visible-string ,get-VisibleString #f)
       (general-string ,get-GeneralString #f)
       (universal-string ,get-UniversalString #f)
       (character-string #f #f)
       (bmp-string ,get-UniversalString #f)
       (reserved #f #f)))                       ;used in get-type

  (define (get-value bv start* end*)
    ;; DER uses a Tag-Length-Value encoding. This procedure
    ;; recursively parses the value at index `start*'.
    (let*-values (((constructed class number startl) (get-type bv start* end*))
                  ((len start) (get-length bv startl end*))
                  ((end) (+ start len)))
      (unless (<= end end*)
        (error 'decode "over the edge now" end end*))
      (if (eq? class 'universal)
          (let* ((decoder (vector-ref universal-types number))
                 (offset (if constructed 2 1))
                 (type-name (car decoder)))
            (print ";" type-name)
            (cond ((list-ref decoder offset) =>
                   (lambda (dec)
                     (values end (list type-name start* (- end start*)
                                       (dec bv start len)))))
                  (else
                   (error 'decode
                          "can't handle this universal value"
                          type-name constructed))))
          ;; Delay parsing implicitly encoded types (their real type
          ;; is in the ASN.1 type, so the translation below must
          ;; handle it). Explicitly tagged values have their tag
          ;; stripped below.
          (values end (list (list (if constructed 'explicit 'implicit) class number)
                            start* (- end start*)
                            (if constructed
                                (car (get-sequence/set bv start len))
                                (subbytevector bv start end)))))))

  (define (decode-implicit type value)
    ;; Given a universal type name, return a decoder procedure.
    (let lp ((i 0))
      (cond ((= i (vector-length universal-types))
             (error 'get-decoder "No decoder for this type" type))
            ((eq? (car (vector-ref universal-types i)) type)
             ((cadr (vector-ref universal-types i))
              value 0 (bytevector-length value)))
            (else (lp (+ i 1))))))

  (define decode
    (case-lambda
      ((bv start end)
       (let-values (((len value) (get-value bv start end)))
         value))
      ((bv)
       (let-values (((len value) (get-value bv 0 (bytevector-length bv))))
         value))))

;;; These procedures take a parse tree and an ASN.1 type and mixes the
;;; two together, taking care of naming fields and handling default
;;; values etc.

  (define field-name car)               ;SEQUENCE fields
  (define field-type cadr)
  (define field-opts cddr)

  (define data-type car)
  (define data-start-index cadr)
  (define data-length caddr)
  (define data-value cadddr)


  (define (find-choice choices type)
    ;; Returns (field-name field-type ...) or #f if there's no match.
    (find
     (lambda (choice)
       (or (eq? (field-type choice) type)
           ;; choice: (dNSName (implicit context 2 ia5-string))
           ;; type: (implicit context 2)
           (and (list? (field-type choice))
                (>= (length (field-type choice)) 3)
                (equal? type (take (field-type choice) 3)))))
     choices))

  ;; TODO: rewrite this function. It's a horrible mess of duplicated
  ;; functionality. And it gets more and more complicated.

  (define (translate-sequence data type format-field)
    (case (car type)
      ((integer)
       ;; An integer with named values
       (unless (eq? (car data) 'integer)
         (error 'translate "expected integer"))
       (let ((names (map (lambda (x) (cons (cdr x) (car x))) (cadr type))))
         (cond ((assv (cadddr data) names) => cdr)
               (else (error 'translate "bad named integer maybe?"
                            (data-value data))))))

      ((choice)
       (let ((f (find-choice (cdr type) (data-type data)))
             (d data))
         (unless f
           (error 'translate "no right choice" type data))
         (print "#;TAKEN-CHOICE " f)
         (print "#;WITH-DATA " d)
         (cond ((and (list? (field-type f)) (list? (data-type d))
                     (eq? 'implicit (car (field-type f)))
                     (eq? 'implicit (car (data-type d)))
                     (eq? (cadr (field-type f)) (cadr (data-type d)))
                     (eq? (caddr (field-type f)) (caddr (data-type d))))
                (let ((implicit-type (list-ref (field-type f) 3)))
                  (format-field (field-name f)
                                (field-type f)
                                (decode-implicit implicit-type (data-value data))
                                (data-start-index data) (data-length data))))

               ((and (list? (field-type f)) (list? (data-type d))
                     (eq? 'explicit (car (field-type f)))
                     (eq? 'explicit (car (data-type d)))
                     (eq? (cadr (field-type f)) (cadr (data-type d)))
                     (eq? (caddr (field-type f)) (caddr (data-type d))))
                (let ((explicit-type (list-ref (field-type f) 3)))
                  (format-field (field-name f)
                                (cadddr (field-type f))
                                (translate (data-value d)
                                           (cadddr (field-type f))
                                           format-field)
                                (data-start-index d)
                                (data-length d))))
               
               (else
                (format-field (field-name f)
                              (field-type f)
                              (translate data (field-type f) format-field)
                              (data-start-index data) (data-length data))))))

      ((sequence-of set-of)
       (unless (or (and (eq? (car type) 'sequence-of) (eq? (car data) 'sequence))
                   (and (eq? (car type) 'set-of) (eq? (car data) 'set)))
         (error 'translate "expected set/sequence" (car data)))
       (unless (<= (cadr type) (length data) (caddr type))
         (error 'translate "too little data for set"))
       (map (cut translate <> (cadddr type)) (data-value data)))

      ((sequence set)
       (print ";SEQUENCE/SET")
       (unless (eq? (car type) (car data))
         (error 'translate "expected set/sequence" type (car data)))
       (let lp ((fields (cdr type))
                (data (data-value data) #;(cadddr data))
                (ret '()))
         (print "---")
         (print "These are the fields: " (length fields) " " fields)
         (print "And this is the data: " data)

         (cond ((null? fields)
                (reverse ret))
               ((and (null? data) (assoc 'default (field-opts (car fields)))) =>
                (lambda (default)
                  ;; There's no more data, but here is a field with a default
                  (print ";using default")
                  (lp (cdr fields)
                      data
                      (cons (format-field (field-name (car fields))
                                          (field-type (car fields))
                                          (cadr default)
                                          #f #f)
                            ret))))
               ((null? data)
                (error 'translate "non-optional data missing" fields))
               (else
                (let ((f (car fields))
                      (d (car data)))
                  (print ";Field:\n  " (field-name f) ", " (field-type f) ", "
                         (field-opts f))
                  (print ";data1:\n  " (data-type d) ", " (data-value d))
                  (cond ((and (list? (field-type f)) (list? (data-type d))
                              (eq? 'explicit (car (field-type f)))
                              (eq? 'explicit (car (data-type d)))
                              (eq? (cadr (field-type f)) (cadr (data-type d)))
                              (eq? (caddr (field-type f)) (caddr (data-type d))))
                         ;; (field-type f): (explicit context number type)
                         ;; (data-type d): (explicit context number)
                         (lp (cdr fields)
                             (cdr data)
                             (cons (format-field (field-name f)
                                                 (cadddr (field-type f))
                                                 (translate (data-value d)
                                                            (cadddr (field-type f))
                                                            format-field)
                                                 (data-start-index d)
                                                 (data-length d))
                                   ret)))

                        ;; CHOICE
                        ((and (list? (field-type f))
                              (eq? (car (field-type f)) 'choice)
                              (find-choice (cdr (field-type f))
                                           (data-type d)))
                         =>
                         (lambda (choice)
                           (lp (cdr fields)
                               (cdr data)
                               (cons (format-field (car choice)
                                                   (cadr choice)
                                                   (translate d (cadr choice)
                                                              format-field)
                                                   (data-start-index d)
                                                   (data-length d))
                                     ret))))

                        ;; FIXME: implicit
                        ((and (list? (field-type f)) (list? (data-type d))
                              (eq? 'implicit (car (field-type f)))
                              (eq? 'implicit (car (data-type d)))
                              (eq? (cadr (field-type f)) (cadr (data-type d)))
                              (eq? (caddr (field-type f)) (caddr (data-type d))))
                         ;; (field-type f): (implicit context number type)
                         ;; (data-type d): (implicit context number)
                         (error 'translate "implicit data handling not implemented"
                                f d))

                        ((or (and (list? (field-type f))
                                  (not (memq (car (field-type f))
                                             '(implicit explicit choice))))
                             (eq? (data-type d) (field-type f))
                             (eq? (field-type f) 'ANY))
                         ;; The field type matches the data type
                         (lp (cdr fields)
                             (cdr data)
                             (cons (format-field (field-name f)
                                                 (field-type f)
                                                 (translate (car data) (field-type f)
                                                            format-field)
                                                 (data-start-index (car data))
                                                 (data-length (car data)))
                                   ret)))

                        ((assoc 'default (field-opts f)) =>
                         (lambda (default)
                           (print ";using default")
                           (lp (cdr fields)
                               data
                               (cons (format-field (field-name f)
                                                   (field-type f)
                                                   (cadr default)
                                                   #f #f)
                                     ret))))
                        (else
                         (error 'translate
                                "unexpected data" (caar data) (field-type f)))))))))
      (else

       (error 'translate "error in ASN.1 type" data type))))

  (define translate
    (case-lambda
      ((data type)
       (translate data type (lambda (name type value start len) value)))
      ((data type format-field)
       (print (list 'translate data type format-field))
       (cond ((list? type)
              (translate-sequence data type format-field))
             ((eq? 'ANY type)
              ;; Delay interpretation. Usually the ANY type is
              ;; combined with an OID which decides the type.
              data)
             ((eq? type (data-type data))
              (data-value data))
             (else
              (error 'translate "unexpected type" type (car data))))))))
