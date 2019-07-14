;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2017-2019 Göran Weinholt <goran@weinholt.se>
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

;; All forms of distributable bundles and analysis go through here.

(library (akku lib bundle)
  (export
    dependency-scan
    license-scan
    compat-scan
    logger:akku.bundle)
  (import
    (rnrs (6))
    (only (srfi :1 lists) append-map filter-map delete-duplicates)
    (only (srfi :67 compare-procedures) <? default-compare)
    (srfi :115 regexp)
    (industria strings)
    (wak fmt)
    (wak fmt color)
    (xitomatl AS-match)
    (akku lib file-parser)
    (only (akku lib install) make-r6rs-library-filenames libraries-directory
          file-list-filename)
    (only (akku lib lock) read-lockfile)
    (only (akku lib fetch) project-source-directory)
    (akku lib repo-scanner)
    (akku lib schemedb)
    (only (akku lib utils) path-join)
    (akku private logging))

(define logger:akku.bundle (make-logger logger:akku 'bundle))
(define log/info (make-fmt-log logger:akku.bundle 'info))
(define log/warn (make-fmt-log logger:akku.bundle 'warning))
(define log/debug (make-fmt-log logger:akku.bundle 'debug))
(define log/trace (make-fmt-log logger:akku.bundle 'trace))

;; Trace the dependencies of `files`, returning a subset of
;; filename->artifact.
(define (trace-dependencies filenames files lib-dir implementations
                            filename->artifact r6rs-lib-name->artifact*
                            only-first?)
  (define used-files (make-hashtable string-hash string=?))
  (define (trace filename file)
    (unless (or (hashtable-ref filename->artifact filename #f)
                (member filename filenames))
      (error 'trace-dependencies "Accidentally added a non-artifact" filename))
    (unless (hashtable-ref used-files filename #f)
      (log/trace "Adding artifact " filename)
      (hashtable-set! used-files filename file)
      ;; Add included files.
      (for-each
       (lambda (asset)
         ;; XXX: Is the artifact really needed?
         (log/trace "Adding asset " (include-reference-realpath asset)
                    " for " filename)
         (hashtable-set! used-files
                         (include-reference-realpath asset)
                         (hashtable-ref filename->artifact
                                        (include-reference-realpath asset)
                                        #f)))
       (artifact-assets file))
      ;; Add imported files.
      (for-each
       (lambda (import)
         (let* ((lib-name (library-reference-name import))
                (candidate-filenames
                 (map (match-lambda
                       ((dir . fn) (path-join lib-dir (path-join dir fn))))
                      (append-map (lambda (impl)
                                    (make-r6rs-library-filenames lib-name impl '()))
                                  (append implementations '(#f))))))
           ;; FIXME: if only-first?, then stop on the first hit.
           ;; Otherwise keep going.
           (unless (find (lambda (fn)
                           (cond ((hashtable-ref filename->artifact fn #f)
                                  => (lambda (library-artifact)
                                       ;; lib-name was mapped to an artifact.
                                       (trace fn library-artifact)))
                                 (else #f)))
                         candidate-filenames)
             (unless (r6rs-builtin-library? lib-name (artifact-implementation file))
               (log/warn "Can not import " lib-name)))))
       (artifact-imports file))))
  (log/trace "Tracing dependencies")
  (for-each trace filenames files)
  used-files)

;; Scan the directory for all artifacts (libraries, included files)
;; and create hashtables for them.
(define (scan-installed-artifacts dir)
  (define filename->artifact (make-hashtable string-hash string=?))
  (define r6rs-lib-name->artifact* (make-hashtable equal-hash equal?)) ;XXX: not needed?
  (for-each
   (lambda (artifact)
     (hashtable-set! filename->artifact
                     (path-join dir (artifact-path artifact))
                     artifact)
     (when (r6rs-library? artifact)
       (hashtable-update! r6rs-lib-name->artifact* (r6rs-library-name artifact)
                          (lambda (acc) (cons artifact acc))
                          '())))
   (find-artifacts dir #f))
  (values filename->artifact r6rs-lib-name->artifact*))

;; Get a list of filenames for all source needed to compile the files.
(define (find-used-source filenames implementations only-first?)
  (assert (for-all file-exists? filenames))
  ;; FIXME: One filename can map to multiple artifacts...
  (let ((files-to-scan (append-map (lambda (filename)
                                     (or (examine-source-file filename filename '())
                                         (error 'find-used-source
                                                "The file is not understood by file-parser"
                                                filename)))
                                   filenames)))
    (let-values ([(filename->artifact r6rs-lib-name->artifact*)
                  (scan-installed-artifacts (libraries-directory))])
      (trace-dependencies filenames files-to-scan (libraries-directory)
                          implementations filename->artifact
                          r6rs-lib-name->artifact* only-first?))))

;; Each file's dependencies is traced by searching through the
;; libraries directory.
(define (dependency-scan filenames implementations)
  (for-each (lambda (x)
              (display x)
              (newline))
            (list-sort string<?
                       (vector->list
                        (hashtable-keys
                         (find-used-source filenames implementations #f))))))

(define (scan-projects-for-artifacts lockfile-location)
  (let ((lib-name->artifact* (make-hashtable equal-hash equal?)))
    (for-each (lambda (artifact)
                (let ((lib-name (cond ((r6rs-library? artifact) (r6rs-library-name artifact))
                                      ((r7rs-library? artifact) (r7rs-library-name artifact))
                                      ((module? artifact) (module-name artifact))
                                      (else #f))))
                  (when lib-name
                    (hashtable-update! lib-name->artifact* lib-name
                                       (lambda (acc) (cons artifact acc))
                                       '()))))
              (append (append-map (lambda (project)
                                    (find-artifacts (project-source-directory project) #f))
                                  (read-lockfile lockfile-location))
                      (find-artifacts "." #f)))
    lib-name->artifact*))

;; Scan the files and their dependencies. Print an analysis of their
;; implementation compatibility.
(define (compat-scan lockfile-location filenames)
  (assert (for-all file-exists? filenames))
  ;; Make a hashtable from library/module names to lists of artifacts
  (let ((lib-name->artifact* (scan-projects-for-artifacts lockfile-location)))
    (define (trace artifact implementation used-artifacts missing-lib-names)
      (unless (hashtable-ref used-artifacts artifact #f)
        (log/trace "Tracing artifact " artifact " for " implementation)
        (hashtable-set! used-artifacts artifact #t)
        ;; Recursively add imported artifacts.
        (for-each
         (lambda (import)
           (let ((lib-name (library-reference-name import)))
             (cond
               ((or (r6rs-builtin-library? lib-name implementation)
                    (equal? lib-name '(akku metadata))
                    (memq implementation (r6rs-library-block-for-implementations lib-name)))
                (log/trace "Library " (wrt lib-name) " is built-in"))
               ((hashtable-ref lib-name->artifact* lib-name #f) =>
                (lambda (artifact*)
                  (cond ((find (lambda (artifact)
                                 (let ((for-impl (artifact-implementation artifact)))
                                   (or (eq? for-impl implementation) (not for-impl))))
                               artifact*)
                         => (lambda (imported)
                              (trace imported implementation
                                     used-artifacts missing-lib-names)))
                        ((hashtable-ref missing-lib-names lib-name #f))
                        (else
                         (hashtable-set! missing-lib-names lib-name #t)
                         (log/debug "Missing import " lib-name " for " implementation)))))
               ((eq? (artifact-implementation artifact) implementation)
                (log/debug "Missing library for " (wrt lib-name) " used in "
                           (artifact-path artifact) ", assuming built-in"))
               (else
                (log/warn "Could not find import " (wrt lib-name) " for " implementation)))))
         (artifact-imports artifact))))
    (let ((files-to-scan (append-map (lambda (filename)
                                       (or (examine-source-file filename filename '())
                                           (error 'find-used-source
                                                  "The file is not understood by file-parser"
                                                  filename)))
                                     filenames)))
      (let ((missing-libs
             (map (lambda (implementation)
                    (define used-artifacts (make-eq-hashtable))
                    (define missing-lib-names (make-hashtable equal-hash equal?))
                    (log/trace "Tracing dependencies for " implementation)
                    (for-each (lambda (artifact)
                                (trace artifact implementation
                                       used-artifacts missing-lib-names))
                              files-to-scan)
                    (cons implementation (hashtable-keys missing-lib-names)))
                  r6rs-implementation-names)))
        (fmt #t "Summary of compatibility support for:" nl
             (fmt-join (lambda (fn) (cat " • " fn nl)) filenames)
             nl)
        (fmt #t "Supported implementations:" nl)
        (for-each (match-lambda
                   [(implementation . #())
                    (fmt #t (fmt-green (cat " ✓ " implementation)) nl)]
                   [_ #f])
                  missing-libs)
        (fmt #t nl "Unsupported implementations:" nl)
        (for-each (match-lambda
                   [(implementation . #()) #f]
                   [(implementation . _)
                    (fmt #t (fmt-red (cat " ✗ " implementation)) nl)])
                  missing-libs)
        (for-each (match-lambda
                   [(implementation . #()) #f]
                   [(implementation . missing)
                    (vector-sort! (lambda (x y) (<? default-compare x y)) missing)
                    (fmt #t (cat nl "Missing libraries for " implementation ":" nl
                                 (fmt-join (lambda (lib) (cat " • " lib nl))
                                           (vector->list missing))))])
                  missing-libs)))))

;; Gather dependencies (as above) and notices and summarize them. It
;; would be great if this printed SPDX documents.
(define (license-scan filenames implementations)
  (define rx-copyright-start
    (rx (w/nocase bow (or "copyright" "(c)" "©")
                  eow)))
  (define rx-copyright-started
    (rx (w/nocase bow (or "public domain" "SPDX-License-Identifier")
                  eow)))
  (define rx-code-start
    (rx (* space)
        (or "(" "#!"
            ";;; Commentary:"
            ";;; Code:"
            ";;; Exported:"
            ";;; Exports:"
            ";; --"
            "; --"
            ";;@"
            ";;>"
            ";;;;;;;;"
            "\f")
        (* any)))
  (define rx-code-line
    (rx (* space) (or "(" "\"") (* any) (or ")" "\"") (* any)))
  (let ((used-source-files (find-used-source filenames implementations 'only-first))
        (filename->project-name (make-hashtable string-hash string=?))
        (project->file* (make-hashtable string-hash string=?)))
    ;; Read the file list.
    (call-with-input-file (file-list-filename)
      (lambda (p)
        (let lp ()
          (unless (port-eof? p)
            (match (string-split (get-line p) #\tab)
              [(filename project-name type . _)
               (hashtable-set! filename->project-name filename project-name)
               (hashtable-update! project->file* project-name
                                  (lambda (acc) (cons (cons filename type) acc))
                                  '())
               (lp)])))))
    ;; Find the projects where files were used.
    (let ((used-project*
           (delete-duplicates
            (vector->list
             (vector-map (lambda (fn)
                           (cond
                             ((member fn filenames) "-")
                             ((hashtable-ref filename->project-name fn #f))
                             (else
                              (error 'license-scan "Could not find file's project name" fn))))
                         (hashtable-keys used-source-files))))))
      (display "Notices automatically gathered by Akku.scm for these files:\n")
      (for-each (lambda (fn) (display " ") (display fn) (newline)) filenames)
      (display "Some files might not have been used in the final binary.\n")
      (for-each
       (lambda (project-name)
         (display (make-string 76 #\=))
         (newline)
         (display "Project: ")
         (display project-name)
         (newline)
         (for-each
          (match-lambda
           [(fn . type)
            (define printed-header #f)
            (define (print-header)
              (unless printed-header
                (set! printed-header #t)
                (newline)
                (display fn)
                (newline)
                (display "--8<---------------cut here---------------start------------->8---\n")))
            (define (print-footer)
              (when printed-header
                (display "--8<---------------cut here---------------end--------------->8---\n")))
            (cond
              ((equal? type "legal-notice-file")
               (print-header)
               (call-with-input-file fn
                 (lambda (p)
                   (display (get-string-all p))))
               (print-footer))
              ((hashtable-ref used-source-files fn #f)
               (call-with-input-file fn
                 (lambda (p)
                   ;; This is weak, but working for everything used by
                   ;; Akku.scm itself. Sometimes more than is needed
                   ;; is copied over.
                   (let lp ((prev-line #f))
                     (unless (port-eof? p)
                       (let lp-restart ((line0 (get-line p))
                                        (prev-line prev-line))
                         (when (and (or (regexp-search rx-copyright-start line0)
                                        (regexp-search rx-copyright-started line0))
                                    (not (regexp-matches rx-code-line line0)))
                           (print-header)
                           (when (and prev-line (regexp-search rx-copyright-started line0))
                             ;; The author was probably on the previous line.
                             (display prev-line)
                             (newline))
                           (display line0)
                           (newline)
                           (let lp-copy ((prev-line prev-line))
                             (let ((line (get-line p)))
                               (cond ((eof-object? line)
                                      #f)
                                     ((regexp-matches rx-code-start line)
                                      (lp-restart line prev-line)) ;end of comment
                                     (else
                                      (display line)
                                      (newline)
                                      (lp-copy line))))))
                         (lp line0))))
                   (print-footer)))
               (unless printed-header
                 (log/info "No copyright notice in used file " fn))))])
          (list-sort (lambda (x y) (string<? (car x) (car y)))
                     (hashtable-ref project->file* project-name '()))))
       (list-sort string<? used-project*))
      (newline)
      (display (make-string 76 #\=))
      (newline)))))
