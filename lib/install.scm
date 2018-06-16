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
    (only (srfi :13 strings) string-index string-prefix?)
    (only (srfi :67 compare-procedures) <? default-compare)
    (prefix (compression tar) tar:)
    (compression xz)
    (hashing sha-2)
    (laesare reader)
    (only (wak fmt) wrt)
    (only (xitomatl common) pretty-print)
    (xitomatl alists)
    (xitomatl AS-match)
    (only (akku lib compat) chmod file-directory?
          file-regular? file-symbolic-link? file-exists/no-follow?
          directory-list delete-directory rename-file)
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

(define (sources-directory*) "src")

(define (sources-directory)
  (path-join (akku-directory) (sources-directory*)))

;; Turns a project name into that works as a directory name.
(define (project-sanitized-name project)
  (sanitized-name (project-name project)))

(define (project-source-directory project)
  (match (project-source project)
    (('directory dir)
     ;; For sources in directories, refer directly to that directory.
     dir)
    (else
     ;; Otherwise a local src directory must be created.
     (path-join (sources-directory) (project-sanitized-name project)))))

(define (binaries-directory)
  (path-join (akku-directory) "bin"))

(define (libraries-directory)
  (path-join (akku-directory) "lib"))

(define (notices-directory project)
  (path-join (path-join (akku-directory) "notices")
             (project-sanitized-name project)))

(define (file-list-filename)
  (path-join (akku-directory) "list"))

(define (project-cache-file project)
  (path-join (cache-directory)
             (string-append (project-sanitized-name project)
                            "-"
                            (cadr (assq 'sha256 (project-content project))))))

(define-record-type project
  (fields name packages source
          installer                     ;for future extensions
          ;; one of these:
          tag revision content)
  (sealed #t)
  (nongenerative))

(define (parse-project spec)
  (let ((name (car (assq-ref spec 'name)))
        (tag (cond ((assq 'tag spec) => cadr) (else #f)))
        (revision (cond ((assq-ref spec 'revision #f) => car) (else #f)))
        (location (assq 'location spec))
        (content (assq-ref spec 'content #f)))
    (assert (not (char=? (string-ref name 0) #\()))
    (match location
      (('location ('directory _))
       #f)
      (else #f))
    (make-project name
                  (cond ((assq 'install spec) => cdr) (else #f))
                  (car (assq-ref spec 'location))
                  (assq-ref spec 'installer '((r6rs)))
                  tag revision content)))

;; Parse a lockfile, returning a list of project records.
(define (read-lockfile lockfile-location)
  (call-with-input-file lockfile-location
    (lambda (p)
      (unless (equal? (read p) '(import (akku format lockfile)))
        (error 'read-lockfile "Invalid lockfile (wrong import)" lockfile-location))
      ;; TODO: More sanity checking. The names need to be
      ;; case-insensitively unique.
      (let lp ((project* '()))
        (match (read p)
          ((? eof-object?)
           project*)
          (('projects . prj*)
           (lp (append (map parse-project prj*) project*)))
          (_ (lp project*)))))))

;; Position the reader to immediately before the given form
(define (reader-skip-to-form reader form-index)
  (let ((tolerant (reader-tolerant? reader)))
    (reader-tolerant?-set! reader #t)
    (let lp ((form-index form-index))
      (unless (zero? form-index)
        (read-datum reader)
        (lp (- form-index 1))))
    (reader-tolerant?-set! reader tolerant)))

;; Verify that the file contents match whatever the content spec says.
(define (verify-file-contents filename content-spec)
  (let ((s (make-sha-256)))
    (call-with-port (open-file-input-port filename)
      (lambda (inp)
        (let lp ()
          (let ((buf (get-bytevector-n inp (* 16 1024))))
            (unless (eof-object? buf)
              (sha-256-update! s buf)
              (lp))))))
    (let ((expected-digest (cadr (assq 'sha256 content-spec)))
          (computed-digest (sha-256->string (sha-256-finish s))))
      (string-ci=? expected-digest
                   computed-digest))))

;; Extracts a tarball into a directory.
(define (extract-tarball tarball directory)
  (call-with-port (open-xz-file-input-port tarball)
    (lambda (tarp)
      (let loop ()
        (let ((hdr (tar:get-header-record tarp)))
          (unless (eof-object? hdr)
            (case (tar:header-typeflag hdr)
              ((regular)
               (check-filename (tar:header-name hdr) #f)
               (let ((output-filename (path-join directory (tar:header-name hdr))))
                 (log/debug "Writing " output-filename)
                 (mkdir/recursive (car (split-path output-filename)))
                 (call-with-port (open-file-output-port output-filename (file-options no-fail))
                   (lambda (outp)
                     (tar:extract-to-port tarp hdr outp)))))
              ((directory)
               (tar:skip-file tarp hdr))
              (else
               (log/trace "Ignoring the entry " (tar:header-name hdr) " of type "
                          (tar:header-typeflag hdr))
               (tar:skip-file tarp hdr)))
            (loop)))))))

;; Fetch a project so that it's available locally.
(define (fetch-project project)
  (let ((srcdir (project-source-directory project)))
    ;; Get the code.
    (log/info "Fetching " (project-name project))
    (match (project-source project)
      (('git repository)
       ;; Git repository
       (cond ((file-directory? srcdir)
              (git-remote-set-url srcdir "origin" repository))
             (else
              (if (project-tag project)
                  (git-shallow-clone srcdir repository)
                  (git-clone srcdir repository))))
       (let ((current-revision (git-rev-parse srcdir "HEAD")))
         (cond ((equal? current-revision (project-revision project)))
               ((project-tag project)
                (git-fetch-tag srcdir (project-tag project))
                (git-checkout-tag srcdir (project-tag project)))
               ((project-revision project)
                (git-fetch srcdir)
                (git-checkout-commit srcdir (project-revision project)))
               (else
                (error 'install "No revision" project))))
       (let ((current-revision (git-rev-parse srcdir "HEAD")))
         (log/info "Fetched revision " current-revision)
         (unless (or (not (project-revision project))
                     (equal? current-revision (project-revision project)))
           (error 'install "Tag does not match revision" (project-tag project)
                  (project-revision project)))))
      (('directory dir)
       ;; Local directory
       (unless (file-directory? dir)
         (error 'install "Directory does not exist" project)))
      (('url url)
       ;; Internet download
       (mkdir/recursive (cache-directory))
       (let* ((cached-file (project-cache-file project))
              (temp-filename (string-append cached-file ".partial")))
         (let retry ((i 2))
           (cond
             ((and (file-exists? cached-file)
                   (verify-file-contents cached-file (project-content project)))
              (log/info "Extracting " cached-file)
              (extract-tarball cached-file srcdir))
             ((zero? i)
              (error 'install "Download failed" url (project-name project)))
             (else
              (log/info "Downloading " url)
              (when (file-exists? temp-filename)
                (delete-file temp-filename))
              (download-file url temp-filename #f)
              (rename-file temp-filename cached-file)
              (retry (- i 1)))))))
      (else
       (error 'install "Unsupported project source: upgrade Akku.scm"
              (project-source project) (project-name project))))))

(define (check-filename filename windows?)
  ;; Protection against path traversal attacks and other types of
  ;; names that would break on some systems. For one particularly
  ;; difficult system, see:
  ;; https://msdn.microsoft.com/en-us/library/windows/desktop/aa365247(v=vs.85).aspx
  ;; TODO: Check for unicode names that break under Windows.
  (define reserved-names
    '("CON" "PRN" "AUX" "NUL" "COM1" "COM2" "COM3" "COM4" "COM5" "COM6" "COM7" "COM8"
      "COM9" "LPT1" "LPT2" "LPT3" "LPT4" "LPT5" "LPT6" "LPT7" "LPT8" "LPT9"))
  (define reserved-chars
    "<>:\"/\\|?*")
  (for-each
   (lambda (component)
     (cond ((string=? component "") ;// does nothing
            (error 'check-filename "Empty path component" filename))
           ((member component '("." ".."))
            (error 'check-filename "Path component is . or .." filename))
           ((string-index component #\nul)
            (error 'check-filename "Path component contains NUL" filename))
           ((and windows?
                 (exists (lambda (part)
                           (exists (lambda (reserved) (string-ci=? part reserved))
                                   reserved-names))
                         (string-split component #\.)))
            (error 'check-filename "Path contains component reserved on MS Windows"
                   filename))
           ((and windows?
                 (exists (lambda (c) (string-index reserved-chars c))
                         (string->list component)))
            (error 'check-filename "Path contains character reserved on MS Windows"
                   filename))
           ((and windows?
                 (exists (lambda (c) (<= 1 (char->integer c) 31))
                         (string->list component)))
            (error 'check-filename "Path contains character disallowed on MS Windows"
                   filename))))
   (string-split filename #\/)))

;; Makes all known variants of the path for the library.
(define (make-r6rs-library-filenames name implementation)
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
               (filename (if implementation
                             (string-append filename "."
                                            (symbol->string implementation))
                             filename))
               (filename (string-append filename ".sls")))
          (check-filename filename (support-windows?))
          (split-path filename))))
    (library-name->file-name-variants implementation))
   equal?))

;; Copies a single R6RS library form from one file to another.
(define (copy-r6rs-library target-directory target-filename source-pathname form-index)
  (let ((target-pathname (path-join target-directory target-filename)))
    (log/debug "Copying R6RS library " source-pathname (if (zero? form-index) "" " ")
           (if (zero? form-index) "" (list 'form form-index))
           " to " target-pathname)
    (check-filename target-pathname (support-windows?))
    (call-with-port (open-input-file source-pathname)
      (lambda (inp)
        (read-shebang inp)
        (let* ((start (port-position inp))
               (f0 (read inp))
               (f1 (read inp))
               (target-pathname (path-join target-directory target-filename)))
          (mkdir/recursive target-directory)
          (when (file-symbolic-link? target-pathname)
            (delete-file target-pathname))
          (call-with-port (open-file-output-port target-pathname
                                                 (file-options no-fail)
                                                 (buffer-mode block)
                                                 (native-transcoder))
            (lambda (outp)
              ;; TODO: Only add #!r6rs if it's not in the original source.
              (display "#!r6rs " outp) ;XXX: required for Racket
              (cond ((and (= form-index 0) (eof-object? f1))
                     ;; The source has a single form, so it's safe to
                     ;; copy the text. TODO: Hardlink in this case. If
                     ;; hardlinks are used then file-symbolic-links?
                     ;; needs to be file-exists/no-follow? everywhere.
                     (set-port-position! inp start)
                     (pipe-ports outp inp))
                    (else
                     ;; TODO: Include comments and original formatting
                     ;; for this case. This will be a problem for
                     ;; license compliance if form 0 is not used in a
                     ;; compiled program, but form 1 is.
                     (log/debug "Reformatting " target-pathname)
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
                       (pretty-print form outp)))))))))
    target-pathname))

;; Write out an R6RS library form.
(define (write-r6rs-library target-directory target-filename source-pathname form)
  (let ((target-pathname (path-join target-directory target-filename)))
    (log/debug "Writing R6RS library to " target-pathname)
    (check-filename target-pathname (support-windows?))
    (let ((target-pathname (path-join target-directory target-filename)))
      (mkdir/recursive target-directory)
      (when (file-symbolic-link? target-pathname)
        (delete-file target-pathname))
      (call-with-port (open-file-output-port target-pathname
                                             (file-options no-fail)
                                             (buffer-mode block)
                                             (native-transcoder))
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
        (let ((reader (make-reader inp source-pathname)))
          (reader-tolerant?-set! reader #t)
          (reader-skip-to-form reader form-index)
          (mkdir/recursive target-directory)
          (when (file-symbolic-link? target-pathname)
            (delete-file target-pathname))
          (call-with-port (open-file-output-port target-pathname
                                                 (file-options no-fail)
                                                 (buffer-mode block)
                                                 (native-transcoder))
            (lambda (outp)
              (display "#!/usr/bin/env scheme-script\n" outp)
              (display ";; Copied by Akku from " outp)
              (write source-pathname outp)
              (display " !#" outp) ;XXX: required for GNU Guile
              (display "\n#!r6rs\n " outp) ;XXX: required for Racket
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
        (call-with-port (open-file-output-port target-pathname
                                               (file-options no-fail))
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

;; Install an R6RS library.
(define (install-artifact/r6rs-library project artifact srcdir always-symlink?)
  (let ((library-locations
         (make-r6rs-library-filenames (r6rs-library-name artifact)
                                      (artifact-implementation artifact))))
    (cond
      ((null? library-locations)
       (log/warn "Could not construct a filename for "
                 (r6rs-library-name artifact))
       '())
      (else
       ;; Create each of the locations for the library. The first
       ;; is a regular file and the rest are symlinks.
       (let ((target (car library-locations))
             (aliases (cdr library-locations)))
         (let ((target-pathname
                (cond ((and always-symlink? (zero? (artifact-form-index artifact))
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
                                          (artifact-form-index artifact))))))
           (cons target-pathname
                 (map-in-order
                  (lambda (alias)
                    (symlink-file (path-join (libraries-directory) (car alias))
                                  (cdr alias)
                                  target-pathname))
                  aliases))))))))

;; Install an R7RS library.
(define (install-artifact/r7rs-library project artifact srcdir always-symlink?)
  (define (read-include source-filename target-filename)
    (read-all-forms (resolve-relative-filename source-filename target-filename) #f))
  (let ((library-locations
         (make-r6rs-library-filenames (r7rs-library-name->r6rs
                                       (r7rs-library-name artifact))
                                      (artifact-implementation artifact))))
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
             (cons target-pathname
                   (map-in-order
                    (lambda (alias)
                      (symlink-file (path-join (libraries-directory) (car alias))
                                    (cdr alias)
                                    target-pathname))
                    aliases)))))))))

;; Install an artifact.
(define (install-artifact project artifact srcdir always-symlink?)
  (cond
    ((r6rs-library? artifact)
     (install-artifact/r6rs-library project artifact srcdir always-symlink?))
    ((or (r6rs-program? artifact)
         (r7rs-program? artifact))      ;TODO: convert the imports in this case
     (cond
       ((or (artifact-internal? artifact) (not (artifact-for-bin? artifact)))
        (log/trace "Skipping the program " (artifact-path artifact))
        '())
       (else
        (let ((target (split-path (artifact-path artifact))))
          (if (and always-symlink? (zero? (artifact-form-index artifact))
                   (artifact-last-form? artifact))
              (list (symlink-file (binaries-directory)
                                  (cdr target)
                                  (path-join srcdir (artifact-path artifact))))
              (list (copy-program (binaries-directory)
                                  (cdr target)
                                  (path-join srcdir (artifact-path artifact))
                                  (artifact-form-index artifact)
                                  (r7rs-program? artifact))))))))
    ((r7rs-library? artifact)
     (install-artifact/r7rs-library project artifact srcdir always-symlink?))
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
       (let* ((artifact* (filter (lambda (artifact)
                                   (not (artifact-for-test? artifact)))
                                 (find-artifacts srcdir #f)))
              (asset* (delete-duplicates
                       (append-map artifact-assets artifact*)
                       (lambda (x y)
                         (equal? (include-reference-path x)
                                 (include-reference-path y))))))
         (let ((artifact-filename*
                (map-in-order (lambda (artifact)
                                (map (lambda (fn) (cons artifact fn))
                                     (install-artifact project artifact srcdir always-symlink?)))
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
  ;; from scheme-ci. Larceny is missing.
  (let ((filename (path-join (binaries-directory) "activate")))
    (log/info "Installing " filename)
    (mkdir/recursive (binaries-directory))
    (call-with-port (open-file-output-port filename
                                           (file-options no-fail)
                                           (buffer-mode block)
                                           (native-transcoder))
      (lambda (p)
        (display "# Load this with \"source .akku/bin/activate\" in bash\n" p)
        (display "export CHEZSCHEMELIBDIRS=\"$PWD/.akku/lib\"\n" p)
        (display "unset CHEZSCHEMELIBEXTS\n" p)
        (display "export GUILE_LOAD_PATH=\"$PWD/.akku/lib\"\n" p)
        (display "export IKARUS_LIBRARY_PATH=\"$PWD/.akku/lib\"\n" p)
        (display "export MOSH_LOADPATH=\"$PWD/.akku/lib\"\n" p)
        (display "export PLTCOLLECTS=\":$PWD/.akku/lib\"\n" p)
        (display "export SAGITTARIUS_LOADPATH=\"$PWD/.akku/lib\"\n" p)
        (display "export VICARE_SOURCE_PATH=\"$PWD/.akku/lib\"\n" p)
        (display "export YPSILON_SITELIB=\"$PWD/.akku/lib\"\n" p)
        (display "export PATH=$PWD/.akku/bin:$PATH\n" p)))))

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
                              (list (r6rs-library-name artifact)))
                             ((r7rs-library? artifact)
                              (list (r7rs-library-name artifact)))
                             (else '()))])
                     artifact/fn*)])
       installed-project/artifact*))))
  (define installed-assets
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
      installed-project/artifact*)))
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
     (make-r6rs-library-filenames library-name #f))))

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

(define (install lockfile-location manifest-filename)
  (let ((project-list (read-lockfile lockfile-location))
        (current-project (make-project "" #f '(directory ".") '((r6rs)) #f #f #f)))
    (mkdir/recursive (akku-directory))
    (mkdir/recursive (libraries-directory))
    (let ((gitignore (path-join (akku-directory) ".gitignore")))
      (unless (file-exists? gitignore)
        (call-with-output-file gitignore
          (lambda (p)
            (display (sources-directory*) p)
            (newline p)))))
    (for-each fetch-project project-list)
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
