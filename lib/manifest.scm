;; -*- mode: scheme; coding: utf-8 -*-
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

;; Manipulate Akku.manifest

(library (akku lib manifest)
  (export
    make-package package?
    package-name package-version*
    package->index-package
    make-version version? parse-version
    version-number version-semver version-lock version-lock-set! version-depends
    version-depends/dev version-conflicts
    version-synopsis version-description version-authors version-homepage version-license
    read-manifest
    write-manifest
    draft-akku-package)
  (import
    (rnrs (6))
    (semver versions)
    (only (spells filesys) rename-file)
    (spdx parser)
    (wak fmt)
    (xitomatl alists)
    (xitomatl AS-match)
    (only (xitomatl common) pretty-print)
    (only (akku lib compat) getcwd)
    (only (akku lib utils) split-path))

(define-record-type package
  (nongenerative)
  (sealed #t)
  (fields name version*))

(define-record-type version
  (nongenerative)
  (sealed #t)
  (fields number semver (mutable lock) depends depends/dev conflicts
          synopsis description authors homepage license))

;; Converts a package record to the format used in package indices.
(define (package->index-package package)
  `(package (name ,(package-name package))
            (versions
             ,@(map (lambda (version)
                      `((version ,(version-number version))
                        (synopsis ,@(version-synopsis version))
                        ,@(if (version-description version)
                              `((description ,@(version-description version)))
                              '())
                        (authors ,@(version-authors version))
                        ,@(if (version-homepage version)
                              `((homepage ,@(version-homepage version)))
                              '())
                        (license ,@(version-license version))
                        (lock ,@(version-lock version))
                        (depends ,@(version-depends version))
                        (depends/dev ,@(version-depends/dev version))
                        (conflicts ,@(version-conflicts version))))
                    (package-version* package)))))

(define (parse-version version-spec)
  (let ((version-number (car (assq-ref version-spec 'version))))
    (make-version version-number (string->semver version-number)
                  ;; For install
                  (assq-ref version-spec 'lock)
                  ;; For lock
                  (assq-ref version-spec 'depends '())
                  (assq-ref version-spec 'depends/dev '())
                  (assq-ref version-spec 'conflicts '())
                  ;; For humans
                  (assq-ref version-spec 'synopsis #f)
                  (assq-ref version-spec 'description #f)
                  (assq-ref version-spec 'authors #f)
                  (assq-ref version-spec 'homepage #f)
                  ;; For someone
                  (assq-ref version-spec 'license #f))))

;; Read the packages in the manifest. Optionally mangle names so they
;; don't get mixed up with names in the index. Optionally override the
;; version number.
(define read-manifest
  (case-lambda
    ((manifest-filename)
     (read-manifest manifest-filename #f #f))
    ((manifest-filename mangle-names? version-override)
     (call-with-input-file manifest-filename
       (lambda (p)
         (let lp ((pkg* '()) (name* '()))
           (match (read p)
             (('akku-package (name version) prop* ...)
              (assert (not (member name name*)))
              (let ((license-expr (car (assq-ref prop* 'license))))
                (or (member license-expr '("NONE" "NOASSERTION"))
                    (parse-license-expression license-expr))) ;TODO: validate
              (let* ((ver (parse-version `((version ,(or version-override version))
                                           (lock #f)
                                           ,@prop*)))
                     (pkg (make-package (if mangle-names?
                                            `(in-manifest: ,name)
                                            name)
                                        (list ver))))
                (lp (cons pkg pkg*) (cons name name*))))
             ((? eof-object?)
              pkg*)
             (else (lp pkg* name*)))))))))   ;allow for future expansion

(define (pretty/1 x)
  (lambda (st)
    ((cat #\( (wrt (car x)) " " (wrt (cadr x)) nl
          (fmt-join
           (lambda (datum)
             (cat (space-to 2) (pretty datum)))
           (cddr x))
          #\) nl)
     st)))

(define (write-manifest manifest-filename akku-package*)
  (call-with-port (open-file-output-port
                   (string-append manifest-filename ".tmp")
                   (file-options no-fail)
                   (buffer-mode block)
                   (native-transcoder))
    (lambda (p)
      (display "#!r6rs ; -*- mode: scheme; coding: utf-8 -*-\n" p)
      (write '(import (akku format manifest)) p)
      (display "\n\n" p)
      (for-each (lambda (pkg)
                  (fmt p (pretty/1 pkg)))
                akku-package*)))
  (rename-file (string-append manifest-filename ".tmp") manifest-filename))

(define (draft-akku-package version . extra*)
  `(akku-package (,(string-downcase (cdr (split-path (getcwd))))
                  ,(or version "0.0.0-alpha.0"))
                 (synopsis "I did not edit Akku.manifest")
                 ;; TODO: Try to get this from git
                 (authors "K. Programistova <schemer@example.com>")
                 (license "NOASSERTION")
                 ,@extra*))

  )