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

;; Tests for (akku lib library-name)

(import
  (rnrs (6))
  (srfi :64 testing)
  (akku lib library-name))

(test-begin "libname-lambda")
(test-equal "/foldling/shorthand/λ"
            (library-name->file-name/chezscheme '(foldling shorthand λ)))
(test-equal "/foldling/shorthand/%ce%bb"
            (library-name->file-name/ikarus '(foldling shorthand λ)))
(test-error (library-name->file-name/ironscheme '(foldling shorthand λ)))
(test-error (library-name->file-name/psyntax '(foldling shorthand λ)))
(test-equal "/foldling/shorthand/%ce%bb"
            (library-name->file-name/racket '(foldling shorthand λ)))
(test-end)

(test-begin "libname-guile")
(test-equal "/srfi"
            (library-name->file-name/guile '(srfi)))
(test-equal "/srfi/srfi-1"
            (library-name->file-name/guile '(srfi :1)))
(test-equal "/srfi/srfi-1"
            (library-name->file-name/guile '(srfi :1 lists)))
(test-equal "/srfi/srfi-1/foo"
            (library-name->file-name/guile '(srfi :1 lists foo)))
(test-end)

(test-begin "libname-larceny")
(test-equal "/srfi"
            (library-name->file-name/larceny '(srfi)))
(test-equal "/srfi/%3a1"
            (library-name->file-name/larceny '(srfi :1)))
(test-equal "/srfi/%3a1/lists"
            (library-name->file-name/larceny '(srfi :1 lists)))
(test-equal "/srfi/%3a1/lists/foo"
            (library-name->file-name/larceny '(srfi :1 lists foo)))
(test-equal "/foldling/shorthand/λ"
            (library-name->file-name/larceny '(foldling shorthand λ)))
(test-end)


(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
