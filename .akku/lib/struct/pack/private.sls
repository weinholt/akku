#!r6rs ;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2008, 2009, 2012, 2017 Göran Weinholt <goran@weinholt.se>

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

;; Auxiliary library for (struct pack). Please don't use this library
;; directly.

(library (struct pack private)
  (export format-size roundb add)
  (import (for (rnrs) (meta -1)))

  (define (add augend addend)
    (if (integer? augend)
        (+ augend addend)
        (with-syntax ((x augend) (y addend))
          #'(+ x y))))

  (define (roundb offset alignment)
    (cond ((integer? offset)
           (bitwise-and (+ offset (- alignment 1))
                        (- alignment)))
          ((and (integer? alignment) (= alignment 1))
           offset)
          (else
           (with-syntax ((x offset))
             #`(bitwise-and (+ x #,(- alignment 1))
                            #,(- alignment))))))

  ;; Find the number of bytes the format requires.
  ;; (format-size "2SQ") => 16
  (define (format-size fmt)
    (define (size c)
      (case c
        ((#\x #\c #\C) 1)
        ((#\s #\S) 2)
        ((#\l #\L #\f) 4)
        ((#\q #\Q #\d) 8)
        (else
         (error 'format-size "Bad character in format string" fmt c))))
    (let lp ((i 0) (s 0) (rep #f) (align #t))
      (cond ((= i (string-length fmt))
             s)
            ((char<=? #\0 (string-ref fmt i) #\9)
             (lp (+ i 1) s
                 (+ (- (char->integer (string-ref fmt i))
                       (char->integer #\0))
                    (* (if rep rep 0) 10))
                 align))
            ((char-whitespace? (string-ref fmt i))
             (lp (+ i 1) s rep align))
            ((char=? (string-ref fmt i) #\a)
             (lp (+ i 1) s rep #t))
            ((char=? (string-ref fmt i) #\u)
             (lp (+ i 1) s rep #f))
            ((memv (string-ref fmt i) '(#\@ #\= #\< #\> #\!))
             (lp (+ i 1) s #f align))
            (else
             (let ((n (size (string-ref fmt i))))
               (lp (+ i 1) (+ (if align (roundb s n) s)
                              (if rep (* n rep) n))
                   #f align)))))))
