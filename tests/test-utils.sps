#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- #!
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

;; Tests for (akku lib utils)

(import
  (rnrs (6))
  (srfi :64 testing)
  (akku lib utils))

(test-begin "symlink/relative")

(test-equal "../../../../private/utils.sls"
            (symlink/relative "./private/utils.sls" ".akku/lib/akku/private/utils.sls" #t))

(test-equal "../../bin/akku.sps"
            (symlink/relative "./bin/akku.sps" ".akku/bin/akku.sps" #t))

(test-equal "../../bin/akku.sps"
            (symlink/relative "./bin/akku.sps" ".akku/bin/akku.sps" #t))

(test-equal "../packrat.sls"
            (symlink/relative ".akku/lib/packrat.sls" ".akku/lib/packrat/main.sls" #t))

(test-end)

(exit (if (zero? (test-runner-fail-count (test-runner-get))) 0 1))
