#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright © 2018 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
#!r6rs

;; Tests for (akku lib r7rs)

(import
  (rnrs (6))
  (srfi :64 testing)
  (akku lib r7rs))

(test-begin "r7rs-parse")
(let ((x (parse-r7rs-define-library '(define-library (empty))
                                    #f #f)))
  (test-assert (r7lib? x))
  (test-equal '(empty) (r7lib-name x))
  (test-equal '() (r7lib-declaration* x)))
(let ((x (parse-r7rs-define-library '(define-library (imp)
                                       (import (rename
                                                (prefix (except (only (scheme base) car cdr)
                                                                cdr)
                                                        scheme:)
                                                (scheme:car car))))
                                    #f #f)))
  (test-equal '(scheme base) (r7import-name (car (r7lib-declaration* x)))))
(let ((x (parse-r7rs-define-library '(define-library (car)
                                       (export car (rename cdr tail))
                                       (import (scheme base)))
                                    #f #f)))
  (test-equal 'car (r7export-internal-name (car (r7lib-declaration* x))))
  (test-equal 'car (r7export-external-name (car (r7lib-declaration* x))))
  (test-equal 'cdr (r7export-internal-name (cadr (r7lib-declaration* x))))
  (test-equal 'tail (r7export-external-name (cadr (r7lib-declaration* x)))))
(let ((x (parse-r7rs-define-library '(define-library (include)
                                       (include-library-declarations
                                        "export.scm" "import.scm"))
                                    "test/r7rs/include.sld"
                                    (lambda (source-filename filename)
                                      (test-equal "test/r7rs/include.sld" source-filename)
                                      (cond
                                        ((string=? filename "export.scm")
                                         '((export car (rename cdr tail))))
                                        ((string=? filename "import.scm")
                                         '((import (scheme base)))))))))
  (test-equal 'car (r7export-internal-name (car (r7lib-declaration* x))))
  (test-equal 'car (r7export-external-name (car (r7lib-declaration* x))))
  (test-equal 'cdr (r7export-internal-name (cadr (r7lib-declaration* x))))
  (test-equal 'tail (r7export-external-name (cadr (r7lib-declaration* x))))
  (test-equal '(scheme base) (r7import-name (caddr (r7lib-declaration* x))))
  (test-equal '() (cdddr (r7lib-declaration* x))))
(test-error (parse-r7rs-define-library '(define-library (infinite-include)
                                          (include-library-declarations "include.scm"))
                                       "test/r7rs/infinite-include.sld"
                                       (lambda (source-filename filename)
                                         (cond
                                           ((string=? filename "include.scm")
                                            '((include-library-declarations "include.scm")))))))
(let ((x (parse-r7rs-define-library '(define-library (include-body)
                                       (include "a.scm" "b.scm")
                                       (include-ci "c.scm"))
                                    "test/r7rs/include-body.sld" #f)))
  (test-equal "test/r7rs/include-body.sld"
              (r7include-source-filename (car (r7lib-declaration* x))))
  (test-equal "a.scm" (r7include-target-filename (car (r7lib-declaration* x))))
  (test-equal "b.scm" (r7include-target-filename (cadr (r7lib-declaration* x))))
  (test-equal "c.scm" (r7include-target-filename (caddr (r7lib-declaration* x))))
  (test-equal #f (r7include-ci? (car (r7lib-declaration* x))))
  (test-equal #t (r7include-ci? (caddr (r7lib-declaration* x)))))
(let ((x (parse-r7rs-define-library '(define-library (ce)
                                       (cond-expand
                                        (r7rs (import (scheme base) (scheme eval)))
                                        (else (import (rnrs)))))
                                    #f #f)))
  (let ((ce (car (r7lib-declaration* x))))
    (test-assert (r7condexp? ce))
    (let ((c0 (car (r7condexp-clause* ce)))
          (c1 (cadr (r7condexp-clause* ce))))
      (test-equal 'r7rs (r7condexp-clause-feature-req c0))
      (test-equal '(and) (r7condexp-clause-feature-req c1))
      (test-assert (r7import? (car (r7condexp-clause-declaration* c0))))
      (test-equal '(scheme eval)
                  (r7import-name (cadr (r7condexp-clause-declaration* c0))))
      (test-assert (r7import? (car (r7condexp-clause-declaration* c1)))))))
(test-end)

(test-begin "r7rs-analysis")
(let ((x (parse-r7rs-define-library '(define-library (ce)
                                       (cond-expand
                                        (r7rs (import (scheme base) (scheme eval)))
                                        (r6rs (import (rnrs)))))
                                    #f #f)))
  (test-equal '(r7rs r6rs) (r7lib-referenced-features x)))
(let ((x (parse-r7rs-define-library '(define-library (ce)
                                       (cond-expand
                                        ((and a b) (import (a)))
                                        ((or c (not d)) (import (c)))
                                        ((library (rnrs)) (import (rnrs)))))
                                    #f #f)))
  (test-equal '(a b c d (library (rnrs))) (r7lib-referenced-features x)))
(let ((x (parse-r7rs-define-library '(define-library (ce)
                                       (cond-expand
                                        (a (cond-expand
                                            (c (import (c)))))
                                        (b (import (b)))))
                                    #f #f)))
  (test-equal '(a c b) (r7lib-referenced-features x)))
(let ((x (parse-r7rs-define-library '(define-library (ce)
                                       (cond-expand
                                        (chibi (cond-expand
                                                ((not chicken)
                                                 (import (c)))))
                                        (guile (import (b)))))
                                    #f #f)))
  (test-equal '(chibi chicken guile) (r7lib-implementation-names x)))
(let ((x (parse-r7rs-define-library '(define-library (ce)
                                       (cond-expand
                                        (chibi (cond-expand
                                                ((not chicken)
                                                 (import (c)))))
                                        (guile (import (b)))))
                                    #f #f)))
  (test-assert (not (r7lib-has-generic-implementation? x))))
(let ((x (parse-r7rs-define-library '(define-library (ce)
                                       (cond-expand
                                        (chibi (import (c)))
                                        (else (import (b)))))
                                    #f #f)))
  (test-assert (r7lib-has-generic-implementation? x)))
(test-end)

(test-begin "r7rs-convert")
(test-equal '(library (empty)
               (export)
               (import))
            (r7rs-library->r6rs-library '(define-library (empty))
                                        #f #f #f))
(test-equal '(library (test)
               (export car)
               (import (scheme base))
               (car '(a . b)))
            (r7rs-library->r6rs-library '(define-library (test)
                                           (import (scheme base))
                                           (export car)
                                           (begin (car '(a . b))))
                                        #f #f #f))
(test-equal '(library (test)
               (export iota)
               (import (srfi :1)))
            (r7rs-library->r6rs-library '(define-library (test)
                                           (import (srfi 1))
                                           (export iota))
                                        #f #f #f))
(test-equal '(library (test)
               (export)
               (import (library (for fun))))
            (r7rs-library->r6rs-library '(define-library (test)
                                           (import (for fun)))
                                        #f #f #f))
(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
