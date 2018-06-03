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

;; Scan a repository and print what was found.

(library (akku lib scan)
  (export
    scan-repository)
  (import
    (rnrs (6))
    (only (srfi :67 compare-procedures) <? default-compare)
    (wak fmt)
    (xitomatl AS-match)
    (akku lib compat)
    (akku lib file-parser)
    (akku lib repo-scanner)
    (akku lib schemedb)
    (akku lib utils)
    (akku private utils)
    (akku private logging))

(define logger:akku.init (make-logger logger:akku 'init))
(define log/info (make-fmt-log logger:akku.init 'info))
(define log/warn (make-fmt-log logger:akku.init 'warning))
(define log/debug (make-fmt-log logger:akku.init 'debug))
(define log/trace (make-fmt-log logger:akku.init 'trace))

(define-record-type package
  (nongenerative)
  (fields name artifacts))

(define (print-package pkg)
  (fmt #f "Package: " (package-name pkg))
  (for-each (lambda (artifact)
              (fmt #f " Artifact: " (artifact-path artifact)))
            (package-artifacts pkg)))

;; Go over all artifacts to find dependencies on libraries that are
;; not built-in and not in the package.
(define (find-library-deps artifact*)
  (define lib-deps (make-hashtable equal-hash equal?))
  (define test-deps (make-hashtable equal-hash equal?))
  (for-each
   (lambda (file)
     (for-each
      (lambda (import)
        (let ((lib-name (library-reference-name import)))
          (unless (or (exists (lambda (file)
                                (or
                                  (and (r7rs-library? file)
                                       (equal? (r7rs-library-name file) lib-name))
                                  (and (r6rs-library? file)
                                       (equal? (r6rs-library-name file) lib-name))))
                              artifact*)
                      (r6rs-builtin-library? lib-name (artifact-implementation file))
                      (and (or (r7rs-library? file) (r7rs-program? file))
                           (r7rs-builtin-library? lib-name (artifact-implementation file)))
                      ;; XXX: For now ignore the library dependencies
                      ;; that are only in implementation-specific
                      ;; artifacts.
                      (artifact-implementation file))
            (log/debug "Adding " lib-name " as external due to " (artifact-path file)
                       (if (artifact-implementation file)
                           (cat " (implementation " (artifact-implementation file) ")")
                           ""))
            (if (artifact-for-test? file)
                (hashtable-set! test-deps lib-name #t)
                (hashtable-set! lib-deps lib-name #t)))))
      (artifact-imports file)))
   artifact*)
  (let ((lib-dep* (vector->list (hashtable-keys lib-deps)))
        (test-dep* (vector->list (hashtable-keys test-deps))))
    (values lib-dep*
            (filter (lambda (lib-name) (not (member lib-name lib-dep*))) test-dep*))))

;; Partition a list of artifacts into packages.
#;
(define (find-packages artifact*)
  (define (library-name->package-name lib-name)
    (define *internal-components* '(internal private compat))
    ;; (a) => (a)
    ;; (a ... <internal> . _) => (a ...)
    ;; (a ... _) => (a ...)
    (cond ((and (pair? lib-name) (null? (cdr lib-name)))
           lib-name)
          (else
           (let lp ((lib-name lib-name)
                    (ret* '()))
             (cond ((null? (cdr lib-name))
                    (reverse ret*))
                   ((or (memq (car lib-name) *internal-components*)
                        (string-prefix? "private-" (symbol->string (car lib-name))))
                    (reverse ret*))
                   (else
                    (lp (cdr lib-name) (cons (car lib-name) ret*))))))))
  (let ((package-names (make-hashtable equal-hash equal?))
        (artifact->pkg-name (make-eq-hashtable)))
    ;; Do a first pass where each artifact is assigned to a package based on its name.
    (for-each
     (lambda (artifact)
       (cond ((and (r6rs-library? artifact) (not (artifact-for-test? artifact)))
              (let* ((lib-name (r6rs-library-name artifact))
                     (package-name (library-name->package-name lib-name)))
                (hashtable-set! package-names package-name #t)
                (hashtable-set! artifact->pkg-name artifact package-name)))
             (else                      ;no package for this, yet
              (hashtable-set! artifact->pkg-name artifact #f))))
     artifact*)
    ;; Artifacts named exactly like a package are moved to that package.
    (for-each
     (lambda (artifact)
       (when (and (r6rs-library? artifact)
                  (hashtable-ref package-names (r6rs-library-name artifact) #f))
         (hashtable-set! artifact->pkg-name artifact (r6rs-library-name artifact))))
     artifact*)
    ;; Subsume unnecessary packages into the parent package. If the
    ;; library (a b) in the package (a) is imported by the library
    ;; (a), then move the library (a b) to the package (a).
    (let lp ((iter-artifact* artifact*) (unchanged #t))
      (cond
        ((null? iter-artifact*)
         (unless unchanged
           (lp artifact* #t)))
        (else
         (let ((artifact (car iter-artifact*)))
           (cond
             ((r6rs-library? artifact)
              (let* ((package-name (hashtable-ref artifact->pkg-name artifact #f))
                     (parent-name (and package-name (reverse (cdr (reverse package-name))))))
                (cond
                  ((and package-name
                        (find (lambda (artifact^)
                                ;; See if artifact^ is the base of artifact's package,
                                ;; and artifact^ imports artifact.
                                (and (r6rs-library? artifact^)
                                     (equal? (r6rs-library-name artifact^) parent-name)
                                     (exists (lambda (import)
                                               (equal? (library-reference-name import)
                                                       (r6rs-library-name artifact)))
                                             (artifact-imports artifact^))))
                              artifact*))
                   => (lambda (parent)
                        (let ((new-package (hashtable-ref artifact->pkg-name parent #f)))
                          (cond ((equal? package-name new-package)
                                 (lp (cdr iter-artifact*) unchanged))
                                (else
                                 ;; Move everything in package-name to parent-name.
                                 (log/debug "Package " package-name " subsumed by " parent-name)
                                 (for-each
                                  (lambda (artifact^)
                                    (when (equal? (hashtable-ref artifact->pkg-name artifact^ #f)
                                                  package-name)
                                      (hashtable-set! artifact->pkg-name artifact^ new-package)))
                                  artifact*)
                                 (lp (cdr iter-artifact*) #f))))))
                  (else
                   (lp (cdr iter-artifact*) unchanged)))))
             (else
              (lp (cdr iter-artifact*) unchanged)))))))
    ;; TODO: assign included files to all packages that include them
    (for-each
     (lambda (artifact)
       (unless (hashtable-ref artifact->pkg-name artifact #f)
         (log/info "Orphan: " (artifact-path artifact) " -- " (artifact-path-list artifact))))
     artifact*)
    ;; Gather the final list of package names.
    (hashtable-clear! package-names)
    (for-each
     (lambda (artifact)
       (cond ((hashtable-ref artifact->pkg-name artifact #f)
              => (lambda (package-name)
                   (hashtable-update! package-names package-name
                                      (lambda (pkg-art*) (cons artifact pkg-art*))
                                      '())))))
     artifact*)

    (let-values (((keys values) (hashtable-entries package-names)))
      (pretty-print keys)
      (vector->list
       (vector-map (lambda (package-name artifact*)
                     (make-package package-name artifact*))
                   keys values)))))

(define (scan-repository base-directory)
  (log/info "Scanning " base-directory)
  ;; Print artifacts
  (let ((artifact* (find-artifacts base-directory (scm-file-list base-directory))))
    (fmt #t (fmt-join (lambda (a) (cat (fmt-artifact a) nl)) artifact*))

    ;; Print dependencies
    (let-values (((lib-deps lib-deps/test) (find-library-deps artifact*)))
      (letrec ((fmt-deps
                (lambda (lib-deps)
                  (fmt-join (lambda (lib-name)
                              (cat fl "- " lib-name nl))
                            (list-sort (lambda (x y) (<? default-compare x y))
                                       lib-deps)))))
        (fmt #t "External library dependencies:" nl
             (fmt-deps lib-deps))
        (unless (null? lib-deps/test)
          (fmt #t nl "External library dependencies for tests:" nl
               (fmt-deps lib-deps/test)))))

)))
