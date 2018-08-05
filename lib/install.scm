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

;; Copies code and notices from projects to .akku/.

(library (akku lib install)
  (export
    install
    libraries-directory file-list-filename
    make-r6rs-library-filenames
    logger:akku.install)
  (import
    (rnrs (6))
    (only (srfi :1 lists) filter-map append-map lset-difference)
    (only (srfi :13 strings) string-index string-prefix?)
    (only (srfi :67 compare-procedures) <? default-compare)
    (prefix (compression tar) tar:)
    (compression xz)
    (hashing sha-2)
    (laesare reader)
    (only (spells filesys) file-directory? file-regular?
          file-symbolic-link? rename-file)
    (wak fmt)
    (only (xitomatl common) pretty-print)
    (xitomatl alists)
    (xitomatl AS-match)
    (only (akku lib compat) chmod file-exists/no-follow?
          directory-list delete-directory)
    (akku lib fetch)
    (akku lib file-parser)
    (akku lib git)
    (akku lib manifest)
    (akku lib lock)
    (akku lib library-name)
    (akku lib r7rs)
    (akku lib repo-scanner)
    (akku lib schemedb)
    (akku lib utils)
    (akku private http)
    (akku private logging))

(define logger:akku.install (make-logger logger:akku 'install))
(define log/info (make-fmt-log logger:akku.install 'info))
(define log/warn (make-fmt-log logger:akku.install 'warning))
(define log/error (make-fmt-log logger:akku.install 'error))
(define log/debug (make-fmt-log logger:akku.install 'debug))
(define log/trace (make-fmt-log logger:akku.install 'trace))

(define (support-windows?)
  #f)

(define (akku-directory)
  ".akku")

(define (binaries-directory)
  (path-join (akku-directory) "bin"))

(define (libraries-directory)
  (path-join (akku-directory) "lib"))

(define (r7rs-libraries-directory)
  (path-join (akku-directory) "lib"))

(define (notices-directory project)
  (path-join (path-join (akku-directory) "notices")
             (project-sanitized-name project)))

(define (file-list-filename)
  (path-join (akku-directory) "list"))

;; Position the reader to immediately before the given form
(define (reader-skip-to-form reader form-index)
  (let ((tolerant (reader-tolerant? reader)))
    (reader-tolerant?-set! reader #t)
    (let lp ((form-index form-index))
      (unless (zero? form-index)
        (read-datum reader)
        (lp (- form-index 1))))
    (reader-tolerant?-set! reader tolerant)))

;; Makes all known variants of the path and name for the library.
;; Returns a list of (directory-name file-name).
(define (make-r6rs-library-filenames name implementation other-impl*)
  (define (make-filenames name implementation allowed-impl*)
    (delete-duplicates
     (filter-map
      (lambda (library-name->file-name)
        (guard (exn
                ((serious-condition? exn)
                 (when (and (message-condition? exn)
                            (irritants-condition? exn))
                   (log/error (condition-message exn) ": "
                              (condition-irritants exn)))
                 #f))
          (let* ((filename (library-name->file-name name))
                 (filename (substring filename 1 (string-length filename)))
                 (filename
                  (if implementation
                      (let ((suffix (string-append "." (symbol->string implementation))))
                        (if (string-suffix? suffix filename)
                            filename   ;already has the suffix
                            (string-append filename suffix)))
                      filename))
                 (filename (string-append filename ".sls")))
            (check-filename filename (support-windows?))
            (split-path filename))))
      (if implementation
          (list (library-name->file-name-variant implementation))
          (map library-name->file-name-variant allowed-impl*)))))
  (let ((block-by-omission (r6rs-library-omit-for-implementations name))
        (block-by-exclusion (r6rs-library-block-for-implementations name)))
    (unless (null? block-by-omission)
      (log/trace "Omitting " name " from implementations " block-by-omission))
    (unless (null? block-by-exclusion)
      (log/trace "Excluding " name " from implementations " block-by-exclusion))
    (cond
      (implementation
       ;; Implementation-specific libraries are never blocked.
       (make-filenames name implementation (list implementation)))
      ((pair? block-by-omission)
       ;; Block installation for some implementations. Done by not
       ;; constructing the special filenames that those
       ;; implementations use for this library.
       (make-filenames name #f (lset-difference eq? r6rs-implementation-names
                                                block-by-omission)))
      ((pair? block-by-exclusion)
       ;; Block installation for some implementations. Done by
       ;; installing with names exclusive to all other known
       ;; implementations.
       (let ((allowed-impl* (lset-difference eq? r6rs-implementation-names
                                             (append other-impl* block-by-exclusion))))
         (delete-duplicates
          (append-map (lambda (impl) (make-filenames name impl allowed-impl*))
                      allowed-impl*))))
      (else
       ;; Install and make available to all implementations, but omit
       ;; special filenames for those that have their own
       ;; implementation.
       (make-filenames name implementation
                       (cons #f (lset-difference eq? r6rs-implementation-names
                                                 other-impl*)))))))

;; Makes all known variants of the path and name for the library.
;; Returns a list of (directory-name file-name).
(define (make-r7rs-library-filenames name)
  (delete-duplicates
   (filter-map
    (lambda (library-name->file-name)
      (guard (exn
              ((serious-condition? exn)
               (when (and (message-condition? exn)
                          (irritants-condition? exn))
                 (log/error (condition-message exn) ": "
                            (condition-irritants exn)))
               #f))
        (let* ((filename (library-name->file-name name))
               (filename (substring filename 1 (string-length filename)))
               (filename (string-append filename ".sld")))
          (check-filename filename (support-windows?))
          (split-path filename))))
    (map library-name->file-name-variant/r7rs r7rs-implementation-names))))

;; Copies a single R6RS library form from one file to another.
(define (copy-r6rs-library target-directory target-filename source-pathname form-index last-form?)
  (let ((target-pathname (path-join target-directory target-filename)))
    (log/debug "Copying R6RS library " source-pathname (if (zero? form-index) "" " ")
           (if (zero? form-index) "" (list 'form form-index))
           " to " target-pathname)
    (check-filename target-pathname (support-windows?))
    (call-with-port (open-input-file source-pathname)
      (lambda (inp)
        (read-shebang inp)
        (let* ((start (port-position inp))
               (target-pathname (path-join target-directory target-filename)))
          (mkdir/recursive target-directory)
          (when (file-symbolic-link? target-pathname)
            (delete-file target-pathname))
          (call-with-output-file/renaming target-pathname
            (lambda (outp)
              ;; TODO: Only add #!r6rs if it's not in the original source.
              (display "#!r6rs " outp) ;XXX: required for Racket
              (cond ((and (= form-index 0) last-form?)
                     ;; The source has a single form, so it's safe to
                     ;; copy the text.
                     (pipe-ports outp inp))
                    (else
                     ;; TODO: Include comments and original formatting
                     ;; for this case. This will be a problem for
                     ;; license compliance if form 0 is not used in a
                     ;; compiled program, but form 1 is.
                     (log/debug "Reformatting " target-pathname)
                     (let* ((f0 (read inp))
                            (f1 (read inp)))
                       (let ((form (case form-index
                                     ((0) f0)
                                     ((1) f1)
                                     (else
                                      (let lp ((form-index (- form-index 2)))
                                        (let ((form (read inp)))
                                          (if (zero? form-index)
                                              form
                                              (lp (- form-index 1)))))))))
                         (display ";; Copyright notices may be found in " outp)
                         (write source-pathname outp)
                         (display "\n;; This file was copied by Akku.scm\n" outp)
                         (pretty-print form outp))))))))))
    target-pathname))

;; Copies a single form from one file to another.
(define (copy-source-form target-directory target-filename source-pathname form-index last-form?)
  (let ((target-pathname (path-join target-directory target-filename)))
    (log/debug "Copying source form " source-pathname (if (zero? form-index) "" " ")
           (if (zero? form-index) "" (list 'form form-index))
           " to " target-pathname)
    (check-filename target-pathname (support-windows?))
    (call-with-port (open-input-file source-pathname)
      (lambda (inp)
        (let ((start (port-position inp))
              (reader (make-reader inp source-pathname)))
          ;; This code works with modules using a foreign lexical
          ;; notation. The general idea is: 1) assume that the reader
          ;; still understands the structure of the file 2) record the
          ;; positions of the start and end of the module form 3)
          ;; seek and copy the characters of the module.
          (reader-tolerant?-set! reader #t)
          (let* ((target-pathname (path-join target-directory target-filename)))
            (mkdir/recursive target-directory)
            (when (file-symbolic-link? target-pathname)
              (delete-file target-pathname))
            (call-with-output-file/renaming target-pathname
              (lambda (outp)
                (cond
                  (last-form?
                   ;; The last form is easy to copy.
                   (reader-skip-to-form reader form-index)
                   (pipe-ports outp inp))
                  (else
                   (log/debug "Reformatting " target-pathname)
                   (display ";; Copyright notices may be found in " outp)
                   (write source-pathname outp)
                   (display "\n;; This file was copied by Akku.scm\n" outp)
                   (let lp ((start-line 1)
                            (start-column 0)
                            (form-index form-index))
                     ;; Read a datum in order to find the start and
                     ;; end positions of the form.
                     (with-exception-handler
                       (lambda (exn)
                         (if (and (warning? exn) (lexical-violation? exn)
                                  (message-condition? exn) (irritants-condition? exn)
                                  (source-condition? exn))
                             (log/trace "Ignoring syntax error in " (wrt source-pathname) ": "
                                        (condition-message exn) " with irritants "
                                        (condition-irritants exn)
                                        " at line " (source-line exn)
                                        ", column " (source-column exn))
                             (raise exn)))
                       (lambda () (read-datum reader)))
                     (let* ((end-line (reader-line reader))
                            (end-column (reader-column reader)))
                       (cond
                         ((= form-index 0)
                          ;; This juggling is necessary because textual
                          ;; port positions can't portably be compared.
                          (log/trace "Copying range " start-line ":" start-column
                                     "-" end-line ":" end-column)
                          (set-port-position! inp start)
                          (let lp ((line 1) (column 0))
                            (unless (and (= line start-line) (= column start-column))
                              (let ((ch (get-char inp)))
                                (if (eqv? ch #\linefeed)
                                    (lp (+ line 1) 0)
                                    (lp line (+ column 1))))))
                          (let lp ((line start-line) (column start-column))
                            (unless (and (= line end-line) (= column end-column))
                              (let ((ch (get-char inp)))
                                (put-char outp ch)
                                (if (eqv? ch #\linefeed)
                                    (lp (+ line 1) 0)
                                    (lp line (+ column 1))))))
                          (newline outp))
                         (else
                          (lp end-line end-column (- form-index 1))))))))))))))
    target-pathname))

;; Same as call-with-output-file, but with no-fail and rename or
;; delete depending on if the target already exists and is identical.
(define (%call-with-output-file/renaming filename transcoder proc)
  (define (files-identical? fn0 fn1)
    (call-with-port (open-file-input-port fn0)
      (lambda (p0)
        (call-with-port (open-file-input-port fn1)
          (lambda (p1)
            (let lp ()
              (let ((b0 (get-bytevector-n p0 4096))
                    (b1 (get-bytevector-n p1 4096)))
                (cond ((and (eof-object? b0) (eof-object? b1))
                       #t)
                      ((not (bytevector=? b0 b1))
                       #f)
                      (else (lp))))))))))
  (let ((tempname (string-append filename ".tmp")))
    (call-with-port (open-file-output-port tempname
                                           (file-options no-fail)
                                           (buffer-mode block)
                                           transcoder)
      proc)
    (if (and (file-exists? filename) (files-identical? filename tempname))
        (delete-file tempname)
        (rename-file tempname filename))))

(define (call-with-output-file/renaming filename proc)
  (%call-with-output-file/renaming filename (native-transcoder) proc))

(define (call-with-binary-output-file/renaming filename proc)
  (%call-with-output-file/renaming filename #f proc))

;; Write out an R6RS library form.
(define (write-r6rs-library target-directory target-filename source-pathname form)
  (let ((target-pathname (path-join target-directory target-filename)))
    (log/debug "Writing R6RS library to " target-pathname)
    (check-filename target-pathname (support-windows?))
    (let ((target-pathname (path-join target-directory target-filename)))
      (mkdir/recursive target-directory)
      (when (file-symbolic-link? target-pathname)
        (delete-file target-pathname))
      (call-with-output-file/renaming target-pathname
        (lambda (outp)
          (display "#!r6rs " outp)      ;XXX: required for Racket
          (when source-pathname
            (display ";; Copyright notices may be found in " outp)
            (write source-pathname outp)
            (newline outp))
          (display ";; This file was written by Akku.scm\n" outp)
          (pretty-print form outp))))
    target-pathname))

;; Copies a program from one file to another.
(define (copy-program target-directory target-filename source-pathname form-index r7rs)
  (let ((target-pathname (path-join target-directory target-filename)))
    (log/debug (if r7rs "Converting R7RS" "Copying R6RS") " program "
               source-pathname (if (zero? form-index) "" " ")
               (if (zero? form-index) "" (list 'form form-index))
               " to " target-pathname)
    (check-filename target-pathname (support-windows?))
    (call-with-port (open-input-file source-pathname)
      (lambda (inp)
        (read-shebang inp)
        (let ((reader (make-reader inp source-pathname)))
          (reader-tolerant?-set! reader #t)
          (reader-skip-to-form reader form-index)
          (mkdir/recursive target-directory)
          (when (file-symbolic-link? target-pathname)
            (delete-file target-pathname))
          (call-with-output-file/renaming target-pathname
            (lambda (outp)
              (display "#!/usr/bin/env scheme-script\n" outp) ;XXX: not liked by plt-r6rs
              (display ";; Copied by Akku from " outp)
              (write source-pathname outp)
              (display " !#" outp) ;XXX: required for GNU Guile
              (display "\n#!r6rs\n" outp) ;XXX: required for Racket
              (cond
                ((not r7rs)
                 (pipe-ports outp inp))
                (else
                 ;; Convert the program from R7RS to R6RS. R7RS § 5.1
                 ;; seems to say import can show up multiple times
                 ;; before the expressions and definitions begin.
                 (reader-tolerant?-set! reader #f)
                 (let lp ((gathered-import* '()))
                   (let ((x (read-datum reader)))
                     (match x
                       [('import import* ...)
                        (lp (append gathered-import* import*))]
                       [x
                        (pretty-print `(import ,@(map r7rs-import-set->r6rs
                                                      gathered-import*))
                                      outp)
                        (unless (eof-object? x)
                          (pretty-print x outp))])))
                 (let lp ()
                   (let ((datum (read-datum reader)))
                     (unless (eof-object? datum)
                       (pretty-print datum outp)
                       (lp)))))))))
        (chmod target-pathname #o755)))
    target-pathname))

;; Copy a regular file.
(define (copy-file target-directory target-filename source-pathname)
  (let ((target-pathname (path-join target-directory target-filename)))
    (log/debug "Copying file " source-pathname " to " target-pathname)
    (check-filename target-pathname (support-windows?))
    (call-with-port (open-file-input-port source-pathname)
      (lambda (inp)
        (mkdir/recursive target-directory)
        (when (file-symbolic-link? target-pathname)
          (delete-file target-pathname))
        (call-with-binary-output-file/renaming target-pathname
          (lambda (outp)
            (pipe-ports outp inp)))))
    target-pathname))

(define (symlink-file target-directory target-filename source-pathname)
  (cond
    ((support-windows?)
     (copy-file target-directory target-filename source-pathname))
    (else
     (let ((target-pathname (path-join target-directory target-filename)))
       (log/debug "Symlinking file " source-pathname " to " target-pathname)
       (check-filename target-pathname (support-windows?))
       (mkdir/recursive target-directory)
       (when (file-exists/no-follow? target-pathname)
         (delete-file target-pathname))
       (symlink/relative source-pathname target-pathname)
       target-pathname))))

;; Creates symlinks to target-pathname from dir + alias-target.
;; Returns the created pathnames, including the target.
(define (make-alias-symlinks target-pathname dir alias-targets)
  (cons target-pathname
        (map-in-order
         (lambda (alias)
           (symlink-file (path-join dir (car alias))
                         (cdr alias)
                         target-pathname))
         alias-targets)))

;; Install an R6RS library.
(define (install-artifact/r6rs-library project artifact related-artifact* srcdir always-symlink?)
  (let* ((other-impl* (filter-map (lambda (artifact^)
                                    (and (not (eq? artifact^ artifact))
                                         (r6rs-library? artifact^)
                                         (or (equal? (r6rs-library-name artifact^)
                                                     (r6rs-library-name artifact))
                                             (equal? (r6rs-library-original-name artifact^)
                                                     (r6rs-library-name artifact)))
                                         (artifact-implementation artifact^)))
                                  related-artifact*))
         (library-locations
          (make-r6rs-library-filenames (r6rs-library-name artifact)
                                       (artifact-implementation artifact)
                                       other-impl*)))
    (cond
      ((null? library-locations)
       (log/warn "Could not construct a filename for "
                 (r6rs-library-name artifact))
       (log/trace "Empty result from "
                  `(make-r6rs-library-filenames ',(r6rs-library-name artifact)
                                                ',(artifact-implementation artifact)
                                                ',other-impl*))
       '())
      ((and (r6rs-library-original-name artifact)
            (memq (artifact-implementation artifact) other-impl*))
       (log/trace "Not installing mangled " (r6rs-library-name artifact)
                  ", specific implementation exists")
       '())
      (else
       ;; Create each of the locations for the library. The first
       ;; is a regular file and the rest are symlinks.
       (let ((target (car library-locations))
             (aliases (cdr library-locations)))
         (let ((target-pathname
                (cond
                  ((r6rs-library-original-name artifact)
                   ;; The library name was mangled; needs a rewrite.
                   (let ((fn (path-join srcdir (artifact-path artifact))))
                     (call-with-input-file fn
                       (lambda (inp)
                         (let ((reader (make-reader inp fn)))
                           (reader-skip-to-form reader (artifact-form-index artifact))
                           (let* ((original-lib (read-datum reader))
                                  (lib `(library ,(r6rs-library-name artifact)
                                          ,@(cddr original-lib))))
                             (write-r6rs-library (path-join (libraries-directory) (car target))
                                                 (cdr target)
                                                 (artifact-path artifact)
                                                 lib)))))))
                  ((and always-symlink? (zero? (artifact-form-index artifact))
                        (artifact-last-form? artifact))
                   ;; It's safe to symlink iff the file has a
                   ;; single form.
                   (symlink-file (path-join (libraries-directory) (car target))
                                 (cdr target)
                                 (path-join srcdir (artifact-path artifact))))
                  (else
                   (when always-symlink?
                     (log/warn "Refusing to symlink multi-form file "
                               (artifact-path artifact)))
                   (copy-r6rs-library (path-join (libraries-directory) (car target))
                                      (cdr target)
                                      (path-join srcdir (artifact-path artifact))
                                      (artifact-form-index artifact)
                                      (artifact-last-form? artifact))))))
           (make-alias-symlinks target-pathname
                                (libraries-directory)
                                aliases)))))))

;; Install an R7RS library.
(define (install-artifact/r7rs-library project artifact srcdir always-symlink?)
  (define (read-include source-filename target-filename)
    (read-all-forms (resolve-relative-filename source-filename target-filename) #f))
  (let ((library-locations
         (make-r6rs-library-filenames (r7rs-library-name->r6rs
                                       (r7rs-library-name artifact))
                                      (artifact-implementation artifact)
                                      '())))
    (cond
      ((null? library-locations)
       (log/warn "Could not construct a filename for " (r7rs-library-name artifact))
       '())
      ((and (artifact-implementation artifact)
            (not (memq (artifact-implementation artifact) r6rs-implementation-names)))
       (log/trace "Skipping " (artifact-implementation artifact)
                  " implementation of " (r7rs-library-name artifact))
       '())
      (else
       (let ((target (car library-locations))
             (aliases (cdr library-locations)))
         (guard (exn
                 ((and (message-condition? exn)
                       (irritants-condition? exn)
                       (lexical-violation? exn)
                       (source-condition? exn))
                  (log/error "Not installing " (wrt target)
                             " due to syntax error: "
                             (condition-message exn) " with irritants "
                             (condition-irritants exn)
                             " at line " (source-line exn)
                             ", column " (source-column exn))
                  '()))
           (let ((target-pathname
                  (let ((fn (path-join srcdir (artifact-path artifact))))
                    (call-with-input-file fn
                      (lambda (inp)
                        (let ((reader (make-reader inp fn)))
                          (reader-skip-to-form reader (artifact-form-index artifact))
                          (reader-mode-set! reader 'r7rs)
                          (cond
                            ((and always-symlink?
                                  (r7rs-implementation-name? (artifact-implementation artifact))
                                  (zero? (artifact-form-index artifact))
                                  (artifact-last-form? artifact))
                             ;; The implementation can handle
                             ;; define-library and the file has a
                             ;; single form.
                             (symlink-file (path-join (libraries-directory) (car target))
                                           (cdr target)
                                           (path-join srcdir (artifact-path artifact))))
                            (else
                             ;; Get the define-library form and convert it to a
                             ;; library form.
                             (let* ((def-lib (read-datum reader))
                                    (lib-dir (car (split-path (artifact-path artifact)))))
                               (let ((lib (r7rs-library->r6rs-library
                                           def-lib fn read-include
                                           (artifact-implementation artifact))))
                                 (write-r6rs-library (path-join (libraries-directory)
                                                                (car target))
                                                     (cdr target)
                                                     (artifact-path artifact)
                                                     lib)))))))))))
             (make-alias-symlinks target-pathname
                                  (libraries-directory)
                                  aliases))))))))

;; Install an R7RS library for use in native R7RS implementations.
(define (install-artifact/r7rs-library/native project artifact srcdir always-symlink?)
  (let ((library-locations (make-r7rs-library-filenames (r7rs-library-name artifact))))
    (cond
      ((null? library-locations)
       (log/warn "Could not construct a filename for " (r7rs-library-name artifact))
       '())
      (else
       (let ((target (car library-locations))
             (aliases (cdr library-locations)))
         (let ((target-pathname
                (let ((fn (path-join srcdir (artifact-path artifact))))
                  (cond
                    ((and always-symlink?
                          (zero? (artifact-form-index artifact))
                          (artifact-last-form? artifact))
                     (symlink-file (path-join (r7rs-libraries-directory) (car target))
                                   (cdr target)
                                   (path-join srcdir (artifact-path artifact))))
                    (else
                     (copy-source-form (path-join (r7rs-libraries-directory)
                                                  (car target))
                                       (cdr target)
                                       (path-join srcdir (artifact-path artifact))
                                       (artifact-form-index artifact)
                                       (artifact-last-form? artifact)))))))
           (make-alias-symlinks target-pathname
                                (r7rs-libraries-directory)
                                aliases)))))))

;; Install an implementation-specific module.
(define (install-artifact/module project artifact srcdir always-symlink?)
  (let ((library-locations
         (make-r6rs-library-filenames (module-name artifact)
                                      (artifact-implementation artifact)
                                      '())))
    (cond
      ((null? library-locations)
       (log/warn "Could not construct a filename for " (module-name artifact))
       '())
      (else
       (let ((target (car library-locations))
             (aliases (cdr library-locations)))
         (let ((target-pathname
                (let ((fn (path-join srcdir (artifact-path artifact))))
                  (cond
                    ((and always-symlink?
                          (zero? (artifact-form-index artifact))
                          (artifact-last-form? artifact))
                     ;; The file has a single form.
                     (symlink-file (path-join (libraries-directory) (car target))
                                   (cdr target)
                                   (path-join srcdir (artifact-path artifact))))
                    (else
                     ;; Copy only the module form.
                     (when always-symlink?
                       (log/warn "Refusing to symlink multi-form module "
                                 (artifact-path artifact)))
                     (copy-source-form (path-join (libraries-directory) (car target))
                                       (cdr target)
                                       (path-join srcdir (artifact-path artifact))
                                       (artifact-form-index artifact)
                                       (artifact-last-form? artifact)))))))
           (make-alias-symlinks target-pathname
                                (libraries-directory)
                                aliases)))))))

;; Install an artifact.
(define (install-artifact project artifact related-artifact* srcdir always-symlink?)
  (cond
    ((r6rs-library? artifact)
     (install-artifact/r6rs-library project artifact related-artifact* srcdir always-symlink?))
    ((or (r6rs-program? artifact)
         (r7rs-program? artifact))      ;TODO: convert the imports in this case
     (cond
       ((or (artifact-internal? artifact) (not (artifact-for-bin? artifact)))
        (log/trace "Skipping the program " (artifact-path artifact))
        '())
       (else
        (let ((target (split-path (artifact-path artifact))))
          (if (and always-symlink? (zero? (artifact-form-index artifact))
                   (artifact-last-form? artifact)
                   (r6rs-program? artifact))
              (list (symlink-file (binaries-directory)
                                  (cdr target)
                                  (path-join srcdir (artifact-path artifact))))
              (list (copy-program (binaries-directory)
                                  (cdr target)
                                  (path-join srcdir (artifact-path artifact))
                                  (artifact-form-index artifact)
                                  (r7rs-program? artifact))))))))
    ((r7rs-library? artifact)
     (append (if (not (artifact-implementation artifact))
                 (install-artifact/r7rs-library/native project artifact srcdir always-symlink?)
                 '())
             (install-artifact/r7rs-library project artifact srcdir always-symlink?)))
    ((and (module? artifact)
          (memq (artifact-implementation artifact)
                '(guile)))
     (install-artifact/module project artifact srcdir always-symlink?))
    ((legal-notice-file? artifact)
     (list (copy-file (path-join (notices-directory project) (artifact-directory artifact))
                      (artifact-filename artifact)
                      (path-join srcdir (artifact-path artifact)))))
    (else '())))

;; Installs an asset, which can be any regular file.
(define (install-asset asset always-symlink?)
  (let ((target (split-path (include-reference-path asset))))
    (list ((if always-symlink? symlink-file copy-file)
           (path-join (libraries-directory) (car target))
           (cdr target)
           (include-reference-realpath asset)))))

;; Install a project and return a alist of artifact/asset => filename.
(define (install-project project always-symlink?)
  (let ((srcdir (project-source-directory project)))
    ;; Copy libraries, programs and assets to the file system. These
    ;; operations are ordered.
    (log/info "Installing " (if (string=? (project-name project) "")
                                "the current project"
                                (project-name project)))
    (cond
      ((equal? (project-installer project) '((r6rs)))
       (let* ((artifact* (find-artifacts srcdir #f))
              (artifact*
               (if (string=? (project-name project) "")
                   artifact*
                   (filter (lambda (artifact)
                             (not (artifact-for-test? artifact)))
                           artifact*)))
              (asset* (delete-duplicates
                       (append-map artifact-assets artifact*)
                       (lambda (x y)
                         (equal? (include-reference-path x)
                                 (include-reference-path y))))))
         (let ((artifact-filename*
                (map-in-order (lambda (artifact)
                                ;; FIXME: Should filter related
                                ;; artifacts by library name.
                                (map (lambda (fn) (cons artifact fn))
                                     (install-artifact project artifact artifact*
                                                       srcdir always-symlink?)))
                              artifact*))
               (asset-filename*
                (map-in-order (lambda (asset)
                                (map (lambda (fn) (cons asset fn))
                                     (install-asset asset always-symlink?)))
                              asset*)))
           (append (apply append artifact-filename*) (apply append asset-filename*)))))
      (else
       (log/error "Installation of " (project-name project) " requires a newer Akku.scm")
       (log/error "No support for the installer " (map car (project-installer project)))
       '()))))

;; Installs an activation script, like Python's virtualenv.
(define (install-activate-script)
  ;; TODO: Setup routines for more Schemes, perhaps take the wrappers
  ;; from scheme-ci.
  (let ((filename (path-join (binaries-directory) "activate")))
    (log/info "Installing " filename)
    (mkdir/recursive (binaries-directory))
    (call-with-port (open-file-output-port filename
                                           (file-options no-fail)
                                           (buffer-mode block)
                                           (native-transcoder))
      (lambda (p)
        (let ((lib (libraries-directory))
              (lib7 (r7rs-libraries-directory)))
          (fmt p
               "# Load this with \"source .akku/bin/activate\" in bash" nl
               ;; R6RS
               "export CHEZSCHEMELIBDIRS=\"$PWD/.akku/lib::$PWD/.akku/libobj\"" nl
               "unset CHEZSCHEMELIBEXTS" nl
               "export GUILE_LOAD_PATH=\"$PWD/.akku/lib\"" nl
               "export IKARUS_LIBRARY_PATH=\"$PWD/.akku/lib\"" nl
               "export MOSH_LOADPATH=\"$PWD/.akku/lib\"" nl
               "export PLTCOLLECTS=\":$PWD/.akku/lib\"" nl
               "export SAGITTARIUS_LOADPATH=\"$PWD/.akku/lib\"" nl
               "export VICARE_SOURCE_PATH=\"$PWD/.akku/lib\"" nl
               "export YPSILON_SITELIB=\"$PWD/.akku/lib\"" nl
               "export LARCENY_LIBPATH=\"$PWD/" lib "\"" nl
               ;; R7RS
               "export CHIBI_MODULE_PATH=\"$PWD/" lib7 "\"" nl
               "export PATH=$PWD/.akku/bin:$PATH" nl))))))

;; Installs a library that contains metadata about all artifacts.
(define (install-metadata installed-project/artifact* manifest-filename)
  (define library-name '(akku metadata))
  (define installed-libraries
    (delete-duplicates
     (list-sort
      (lambda (x y) (<? default-compare x y))
      (append-map
       (match-lambda
        [#(project artifact/fn*)
         (append-map (match-lambda
                      [(artifact . _filename)
                       (cond ((r6rs-library? artifact)
                              (list (or (r6rs-library-original-name artifact)
                                        (r6rs-library-name artifact))))
                             ((r7rs-library? artifact)
                              (list (r7rs-library-name artifact)))
                             (else '()))])
                     artifact/fn*)])
       installed-project/artifact*))))
  (define installed-assets
    (delete-duplicates
     (list-sort
      (lambda (x y) (<? default-compare x y))
      (append-map
       (match-lambda
        [#(project artifact/fn*)
         (append-map
          (match-lambda
           [(artifact . _filename)
            (if (not (artifact? artifact))
                '()
                ;; Group by original include form
                (let ((include-spec*
                       (delete-duplicates
                        (map include-reference-original-include-spec
                             (artifact-assets artifact)))))
                  (map (lambda (original-include-spec)
                         (list original-include-spec
                               (filter-map
                                (lambda (asset)
                                  (and (equal? original-include-spec
                                               (include-reference-original-include-spec asset))
                                       (include-reference-path asset)))
                                (artifact-assets artifact))
                               (cond ((r6rs-library? artifact)
                                      (r6rs-library-name artifact))
                                     ((r7rs-library? artifact)
                                      (r7rs-library-name artifact))
                                     (else #f))))
                       include-spec*)))])
          artifact/fn*)])
       installed-project/artifact*))))
  (log/debug "Writing metadata to " library-name)
  (let-values (((main-package-name main-package-version)
                (if (file-exists? manifest-filename)
                    (let ((pkg* (read-manifest manifest-filename)))
                      (values (package-name (car pkg*))
                              (version-number (car (package-version* (car pkg*))))))
                    (values #f #f))))
    (map-in-order
     (lambda (target)
       (cons (make-generic-file "" '())
             (write-r6rs-library
              (path-join (libraries-directory) (car target))
              (cdr target)
              #f
              `(library ,library-name
                 (export main-package-name main-package-version
                         installed-libraries installed-assets)
                 (import (only (rnrs) define quote))
                 (define main-package-name ',main-package-name)
                 (define main-package-version ',main-package-version)
                 (define installed-libraries ',installed-libraries)
                 (define installed-assets ',installed-assets)))))
     (make-r6rs-library-filenames library-name #f '()))))

;; Create .akku/list
(define (install-file-list installed-project/artifact*)
  (define printed-files (make-hashtable string-hash string=?))
  (define (installed-type a)
    (cond ((r6rs-library? a) 'r6rs-library)
          ((r7rs-library? a) 'r7rs-library)
          ((r6rs-program? a) 'r6rs-program)
          ((module? a) 'module)
          ((legal-notice-file? a) 'legal-notice-file)
          ((include-reference? a) 'included-file)
          ((generic-file? a) 'generic-file)
          ((artifact? a) 'artifact)
          (else 'unknown)))
  (let ((filename (file-list-filename)))
    (log/info "Writing " filename)
    (mkdir/recursive (akku-directory))
    (call-with-port (open-file-output-port filename
                                           (file-options no-fail)
                                           (buffer-mode block)
                                           (native-transcoder))
      (lambda (p)
        (for-each
         (match-lambda
          (#(project artifact/fn*)
           (for-each
            (match-lambda
             ((artifact . filename)
              (cond ((hashtable-ref printed-files filename #f)
                     => (match-lambda
                         ((other-project . other-artifact)
                          (letrec ((fmt-name
                                    (lambda (name)
                                      (if (string=? name "")
                                          "the current project"
                                          name))))
                            (log/info "File " filename  " in "
                                      (fmt-name (project-name other-project))
                                      " shadows that from "
                                      (fmt-name (project-name project)))))))
                    (else
                     (hashtable-set! printed-files filename (cons project artifact))
                     (display filename p)
                     (display #\tab p)
                     (display (if (equal? (project-name project) "")
                                  "-"
                                  (project-name project))
                              p)
                     (display #\tab p)
                     (display (installed-type artifact) p)
                     (display #\tab p)
                     (newline p)))))
            (reverse artifact/fn*))))
         (reverse installed-project/artifact*))))))

(define (remove-extraneous-files installed-project/artifact*)
  (let ((current-filenames (make-hashtable string-hash string=?)))
    ;; Gather the filenames of all currently installed projects.
    (for-each
     (match-lambda
      [#(project artifact/fn*)
       (for-each
        (match-lambda
         [(artifact . filename)
          (hashtable-set! current-filenames filename #t)])
        artifact/fn*)])
     installed-project/artifact*)
    ;; Walk over .akku/lib and remove everything not current.
    (letrec ((recurse
              (lambda (path filename*)
                (define (handle-file fn)
                  (let ((filename (path-join path fn)))
                    (cond
                      ((or (file-regular? filename)
                           (file-symbolic-link? filename))
                       (when (not (hashtable-ref current-filenames filename #f))
                         (log/info "Removing uninstalled file " filename)
                         (assert (string-prefix? (libraries-directory) filename))
                         (delete-file filename)))
                      ((file-directory? filename)
                       ;; XXX: must be after the file-symbolic-link?
                       ;; check to prevent this from recursing into
                       ;; directories outside .akku/lib via symlinks.
                       (recurse filename (directory-list filename))
                       (when (null? (directory-list filename))
                         (log/info "Removing empty directory " filename)
                         (assert (string-prefix? (libraries-directory) filename))
                         (delete-directory filename))))))
                (for-each handle-file filename*))))
      (recurse (libraries-directory) (directory-list (libraries-directory))))))

;; Install all projects, assuming that fetch already ran.
(define (install lockfile-location manifest-filename)
  (let ((project-list (read-lockfile lockfile-location))
        (current-project (make-project "" #f '(directory ".") '((r6rs)) #f #f #f)))
    (mkdir/recursive (libraries-directory))
    (let* ((installed-project/artifact*
            (map-in-order (lambda (project)
                            (vector project (install-project project #f)))
                          project-list))
           (installed-project/artifact*
            (if (running-from-home?)    ;protect against user being in $HOME
                installed-project/artifact*
                (append installed-project/artifact*
                        (list (vector current-project
                                      (install-project current-project 'symlink))))))
           (complete-list (append installed-project/artifact*
                                  (list
                                   (vector current-project
                                           (install-metadata installed-project/artifact*
                                                             manifest-filename))))))
      (install-file-list complete-list)
      (remove-extraneous-files complete-list))
    (install-activate-script))))
