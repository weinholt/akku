;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2017-2018 Göran Weinholt <goran@weinholt.se>
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

;; Scheme implementation database.

(library (akku lib schemedb)
  (export
    r6rs-builtin-library?
    r6rs-library-name->implementation-name
    r6rs-library-name*->implementation-name
    r7rs-builtin-library?)
  (import
    (rnrs (6))
    (xitomatl AS-match))

;; True if lib-name is a built-in library provided by the implementation.
(define (r6rs-builtin-library? lib-name implementation-name)
  (or (member lib-name r6rs-standard-libraries)
      (cond
        ((assq implementation-name implementation-specific-libraries)
         => (lambda (impl-spec)
              (let ((lib-pattern* (cdr impl-spec)))
                (exists (lambda (lib-pattern)
                          (match (list lib-name lib-pattern)
                            (((name0 . _) (name1 '*))
                             (eq? name0 name1))
                            (else
                             (equal? lib-name lib-pattern))))
                        lib-pattern*))))
        (else #f))))

(define (r7rs-builtin-library? lib-name)
  (member lib-name r7rs-standard-libraries))

(define implementation-specific-libraries
  '((chezscheme (scheme *)
                (chezscheme *))
    (chibi (chibi))
    (guile (guile *))
    (ikarus (ikarus *))
    (ironscheme (ironscheme *))
    (kawa (kawa *)
          (class *))
    (larceny (primitives *)
             (larceny *)
             (rnrs eval reflection))    ;van Tonder macros
    (mosh (core *)
          (mosh *)
          (nmosh *)
          (primitives *)
          (system))
    (mzscheme (scheme *))
    (sagittarius (sagittarius *))
    (vicare (ikarus *)
            (psyntax *)
            (vicare *))
    (ypsilon (core *)
             (time))))

(define r6rs-standard-libraries
  '((rnrs)
    (rnrs r5rs)
    (rnrs control)
    (rnrs eval)
    (rnrs mutable-pairs)
    (rnrs mutable-strings)
    (rnrs programs)
    (rnrs syntax-case)
    (rnrs files)
    (rnrs sorting)
    (rnrs base)
    (rnrs lists)
    (rnrs io simple)
    (rnrs bytevectors)
    (rnrs unicode)
    (rnrs exceptions)
    (rnrs arithmetic bitwise)
    (rnrs arithmetic fixnums)
    (rnrs arithmetic flonums)
    (rnrs hashtables)
    (rnrs io ports)
    (rnrs enums)
    (rnrs conditions)
    (rnrs records inspection)
    (rnrs records procedural)
    (rnrs records syntactic)))

(define r7rs-standard-libraries
  '((scheme base)
    (scheme case-lambda)
    (scheme char)
    (scheme complex)
    (scheme cxr)
    (scheme eval)
    (scheme file)
    (scheme inexact)
    (scheme lazy)
    (scheme load)
    (scheme process-context)
    (scheme read)
    (scheme repl)
    (scheme time)
    (scheme write)
    (scheme r5rs)))

;; Takes a library name and returns the name of the implementation
;; that supports it. If it's a portable library, then returns #f.
;; Uses the same names as in the .sls prefixes.
(define (r6rs-library-name->implementation-name lib-name)
  ;; TODO: Can be more accurate by knowing the names of identifiers
  ;; which are part of except/only/rename.
  (match lib-name
    (('chezscheme . _) 'chezscheme)
    (('scheme) 'chezscheme)             ;pretty common, legacy
    (('guile . _) 'guile)
    (('ikarus . _) 'ikarus)
    (('ironscheme . _) 'ironscheme)
    (('mosh . _) 'mosh)
    (('nmosh . _) 'mosh)
    (('sagittarius . _) 'sagittarius)
    (('vicare . _) 'vicare)
    (else #f)))

;; Takes a list of library names and determines which implementation
;; supports them.
(define (r6rs-library-name*->implementation-name lib-name*)
  (and (pair? lib-name*)
       (or (r6rs-library-name->implementation-name (car lib-name*))
           (r6rs-library-name*->implementation-name (cdr lib-name*))))))
