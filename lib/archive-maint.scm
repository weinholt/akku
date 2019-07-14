;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2018, 2019 Göran Weinholt <goran@weinholt.se>
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

;; Akku archive maintenance (for central repositories).

(library (akku lib archive-maint)
  (export
    archive-scan)
  (import
    (rnrs (6))
    (semver versions)
    (only (srfi :13 strings) string-suffix?)
    (only (srfi :67 compare-procedures) <? default-compare)
    (wak fmt)
    (chibi match)
    (only (akku private compat) directory-list pretty-print)
    (only (akku lib utils) path-join))

;; Scan a list of directories and print an archive index. It is
;; assumed that the *.akku files have been vetted and that all
;; *.akku.sig files have been verified.
(define (archive-scan directory*)
  (define packages (make-hashtable equal-hash equal?))
  (define (scan-directory dir)
    (define (scan-file fn)
      (when (string-suffix? ".akku" fn)
        (call-with-input-file (path-join dir fn)
          (lambda (p)
            (match (read p)
              [('package ('name name) ('versions version-spec))
               (hashtable-update! packages name
                                  (lambda (version-spec*)
                                    (cons version-spec version-spec*))
                                  '())])))))
    (for-each scan-file (directory-list dir)))
  (for-each scan-directory directory*)
  ;; Print a sorted archive
  (fmt #t "#!r6rs ; -*- mode: scheme; coding: utf-8 -*-" nl
       ";; SPDX-License-Identifier: CC0-1.0" nl
       (wrt '(import (akku format index))) nl nl)
  (let ((package-names (hashtable-keys packages)))
    (vector-sort! (lambda (x y) (<? default-compare x y)) package-names)
    (vector-for-each
     (lambda (pkg-name)
       (let ((versions        ;sort versions in semver order (required)
              (list-sort
               (lambda (v0 v1)
                 (letrec ((get-semver (lambda (v)
                                        (match v
                                          [(('version ver) . _)
                                           (string->semver ver)]))))
                   (<? semver-compare (get-semver v0) (get-semver v1))))
               (hashtable-ref packages pkg-name 'no-versions))))
         (pretty-print
          `(package (name ,pkg-name)
                    (versions ,@versions)))
         (newline)))
     package-names))))
