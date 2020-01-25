;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2017-2020 Göran Weinholt <goran@weinholt.se>
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
    akku-directory libraries-directory ffi-libraries-directory
    r7rs-libraries-directory binaries-directory
    file-list-filename
    make-r6rs-library-filenames
    logger:akku.install)
  (import
    (rnrs (6))
    (only (srfi :1 lists) filter-map append-map lset-difference)
    (only (srfi :13 strings) string-index string-prefix?)
    (only (srfi :67 compare-procedures) <? default-compare)
    (chibi match)
    (prefix (compression tar) tar:)
    (compression xz)
    (hashing sha-2)
    (laesare reader)
    (laesare writer)
    (wak fmt)
    (only (akku private compat) chmod file-exists/no-follow?
          directory-list delete-directory os-name chmod
          pretty-print file-directory? file-regular?
          file-symbolic-link? rename-file)
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
  (memq (os-name) '(cygwin msys)))

(define (akku-directory)
  ".akku")

(define (binaries-directory)
  (path-join (akku-directory) "bin"))

(define (libraries-directory)
  (path-join (akku-directory) "lib"))

(define (r7rs-libraries-directory)
  (path-join (akku-directory) "lib"))

(define (ffi-libraries-directory)
  (path-join (akku-directory) "ffi"))

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

;; Lookahead for all comment tokens from the reader, while keeping the
;; port position.
(define (reader-lookahead-comments reader port)
  (let ((tolerant (reader-tolerant? reader))
        (pos (port-position port)))
    (dynamic-wind
      (lambda ()
        (reader-tolerant?-set! reader #t))
      (lambda ()
        (let lp ((tokens '()))
          (let-values ([(type lexeme) (get-token reader)])
            (cond
              ((or (eof-object? lexeme)
                   (not (memq type '(directive inline-comment whitespace shebang
                                               comment nested-comment))))
               (reverse tokens))
              (else
               (lp (cons (cons type lexeme) tokens)))))))
      (lambda ()
        (reader-tolerant?-set! reader tolerant)
        (set-port-position! port pos)))))

;; Makes all known variants of the path and name for the library.
;; Returns a list of (directory-name file-name).
(define (make-r6rs-library-filenames name implementation other-impl*)
  (define (make-filenames name implementation allowed-impl*)
    (delete-duplicates
     (filter-map
      (lambda (library-name->file-name)
        (guard (exn
                ((serious-condition? exn)
                 (when (and (who-condition? exn)
                            (message-condition? exn)
                            (irritants-condition? exn))
                   (log/debug "(" (condition-who exn) ") "
                              (condition-message exn) ": "
                              (condition-irritants exn))
                   (log/info "The name " (wrt name) " has been rejected by "
                             (condition-who exn)))
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
      (log/debug "Omitting " name " from implementations " block-by-omission))
    (unless (null? block-by-exclusion)
      (log/debug "Excluding " name " from implementations " block-by-exclusion))
    (cond
      (implementation
       ;; Implementation-specific library.
       (if (or (memq implementation block-by-omission)
               (memq implementation block-by-exclusion))
           '()
           (make-filenames name implementation (list implementation))))
      ((pair? block-by-exclusion)
       ;; Block installation for some implementations. Done by
       ;; installing with names exclusive to all other known
       ;; implementations.
       (let ((allowed-impl* (lset-difference eq? r6rs-implementation-names
                                             (append other-impl* block-by-exclusion
                                                     block-by-omission))))
         (delete-duplicates
          (append-map (lambda (impl) (make-filenames name impl allowed-impl*))
                      allowed-impl*))))
      ((pair? block-by-omission)
       ;; Block installation for some implementations. Done by not
       ;; constructing the special filenames that those
       ;; implementations use for this library.
       (make-filenames name #f (lset-difference eq? r6rs-implementation-names
                                                block-by-omission)))
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
               (when (and (who-condition? exn)
                          (message-condition? exn)
                          (irritants-condition? exn))
                 (log/debug "(" (condition-who exn) ") "
                            (condition-message exn) ": "
                            (condition-irritants exn))
                 (log/info "The name " (wrt name) " has been rejected by "
                           (condition-who exn)))
               #f))
        (let* ((filename (library-name->file-name name))
               (filename (substring filename 1 (string-length filename)))
               (filename (string-append filename ".sld")))
          (check-filename filename (support-windows?))
          (split-path filename))))
    (map library-name->file-name-variant/r7rs r7rs-implementation-names))))

;; Find the #!r6rs directive in the reader. Stops at the first form.
;; Meant to see if the source code already has #!r6rs before the
;; library form.
(define (reader-find-r6rs-directive reader inp)
  (let ((start (port-position inp)))
    (let ((directive-seen
           (let loop ()
             (let-values ([(type token) (get-token reader)])
               (cond ((eof-object? token) #f)
                     ((memq type '(openp openb)) #f)
                     ((and (eq? type 'directive)
                           (eq? token 'r6rs))
                      #t)
                     (else
                      (loop)))))))
      ;; Rewind the port
      (set-port-position! inp start)
      directive-seen)))

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
        (let* ((reader (make-reader inp source-pathname))
               (target-pathname (path-join target-directory target-filename))
               (comment-tokens (reader-lookahead-comments reader inp)))
          (mkdir/recursive target-directory)
          (when (file-symbolic-link? target-pathname)
            (delete-file target-pathname))
          (reader-skip-to-form reader form-index)
          (call-with-output-file/renaming target-pathname
            (lambda (outp)
              (let ((writer (make-writer outp target-pathname)))
                ;; Copy the comments first, as they may contain
                ;; copyright notices
                (unless (= form-index 0)
                  (for-each (lambda (t+t)
                              (put-token writer (car t+t) (cdr t+t)))
                            comment-tokens))
                (cond (last-form?
                       ;; XXX: The #!r6rs directive is needed by Racket.
                       ;; Mosh needs a newline after the directive.
                       (unless (reader-find-r6rs-directive reader inp)
                         (display "#!r6rs\n" outp))
                       ;; The source has a single form, so it's safe to
                       ;; copy the text.
                       (pipe-ports outp inp))
                      (else
                       (log/debug "Reformatting " target-pathname)
                       (put-token writer 'directive 'r6rs)
                       (put-token writer 'whitespace "\n")
                       (display ";; Akku.scm extracted this library from " outp)
                       (write source-pathname outp)
                       (newline outp)
                       (copy-form-from-reader-to-writer reader writer 0)))))))))
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
(define (write-r6rs-library target-directory target-filename source-pathname
                            tokens form)
  (let ((target-pathname (path-join target-directory target-filename)))
    (log/debug "Writing R6RS library to " target-pathname)
    (check-filename target-pathname (support-windows?))
    (let ((target-pathname (path-join target-directory target-filename)))
      (mkdir/recursive target-directory)
      (when (file-symbolic-link? target-pathname)
        (delete-file target-pathname))
      (call-with-output-file/renaming target-pathname
        (lambda (outp)
          (display "#!r6rs\n" outp)     ;XXX: required for Racket
          (cond (source-pathname
                 (display ";; Akku.scm wrote this file based on " outp)
                 (write source-pathname outp)
                 (newline outp))
                (else
                 (display ";; This file was written by Akku.scm\n" outp)))
          (let ((writer (make-writer outp target-pathname)))
            (for-each (lambda (t+t)
                        (put-token writer (car t+t) (cdr t+t)))
                      tokens))
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

;; Equivalent to (write (read inp) outp)
(define (copy-form-from-reader-to-writer reader writer current-depth)
  (let loop ((depth current-depth))
    (let-values ([(type token) (get-token reader)])
      (unless (eof-object? token)
        (case type
          ((openp openb vector bytevector)
           (put-token writer type token)
           (loop (+ depth 1)))
          ((closep closeb)
           (put-token writer type token)
           (if (<= depth 1)
               (put-token writer 'whitespace "\n")
               (loop (- depth 1))))
          (else
           (put-token writer type token)
           (loop depth)))))))

;; Read an R6RS library from the reader and write it to a file, while
;; renaming the library.
(define (rewrite-r6rs-library target-directory target-filename source-pathname
                              new-library-name comment-tokens reader)
  (let ((target-pathname (path-join target-directory target-filename)))
    (log/debug "Writing renamed R6RS library to " target-pathname)
    (check-filename target-pathname (support-windows?))
    (let ((target-pathname (path-join target-directory target-filename)))
      (mkdir/recursive target-directory)
      (when (file-symbolic-link? target-pathname)
        (delete-file target-pathname))
      (call-with-output-file/renaming target-pathname
        (lambda (outp)
          (let ((writer (make-writer outp target-pathname)))
            (writer-mode-set! writer 'r6rs)
            (for-each (lambda (t+t)
                        (put-token writer (car t+t) (cdr t+t)))
                      comment-tokens)
            (let loop ((directive-seen #f))
              (let-values ([(type token) (get-token reader)])
                (unless (eof-object? token)
                  (cond ((and (eq? type 'identifier) (eq? token 'library))
                         ;; Write the new library name.
                         (put-token writer type token)
                         (put-token writer 'whitespace " ")
                         (read-datum reader)
                         (write new-library-name outp))
                        ((and (memq type '(openp openb))
                              (not directive-seen))
                         ;; Insert the #!r6rs token if it was not
                         ;; seen. It is required for Racket.
                         (put-token writer 'directive 'r6rs)
                         (put-token writer 'whitespace "\n")
                         (put-token writer type token)
                         (loop #t))
                        (else
                         (put-token writer type token)
                         (loop (or directive-seen
                                   (and (eq? type 'directive)
                                        (eq? token 'r6rs)))))))))
            ;; Copy the rest of the library
            (copy-form-from-reader-to-writer reader writer 1)))))
    target-pathname))

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
       (log/debug "No library location for "
                  (r6rs-library-name artifact)
                  (cond ((artifact-implementation artifact) =>
                         (lambda (impl) (cat " for " impl)))
                        (else "")))
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
                         (let* ((reader (make-reader inp fn))
                                (comment-tokens
                                 (if (zero? (artifact-form-index artifact))
                                     '()  ;comments will be copied anyway
                                     (reader-lookahead-comments reader inp))))
                           (reader-skip-to-form reader (artifact-form-index artifact))
                           (rewrite-r6rs-library (path-join (libraries-directory) (car target))
                                                 (cdr target)
                                                 (artifact-path artifact)
                                                 (r6rs-library-name artifact)
                                                 comment-tokens reader))))))
                  ((and always-symlink? (zero? (artifact-form-index artifact))
                        (artifact-last-form? artifact))
                   ;; It's safe to symlink iff the file has a
                   ;; single form.
                   (symlink-file (path-join (libraries-directory) (car target))
                                 (cdr target)
                                 (path-join srcdir (artifact-path artifact))))
                  (else
                   (when always-symlink?
                     (log/info "The multi-form file "
                               (artifact-path artifact) " will not be symlinked"))
                   (guard (exn
                           ((lexical-violation? exn)
                            (log/debug "Lexical violation in "
                                       (path-join srcdir (artifact-path artifact))
                                       " at line " (source-line exn)
                                       ", column " (source-column exn)
                                       "; using copy-source-form")
                            (copy-source-form (path-join (libraries-directory) (car target))
                                              (cdr target)
                                              (path-join srcdir (artifact-path artifact))
                                              (artifact-form-index artifact)
                                              (artifact-last-form? artifact))))
                     (copy-r6rs-library (path-join (libraries-directory) (car target))
                                        (cdr target)
                                        (path-join srcdir (artifact-path artifact))
                                        (artifact-form-index artifact)
                                        (artifact-last-form? artifact)))))))
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
                  (log/info "Not installing " (wrt (path-join (car target) (cdr target)))
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
                        (let* ((reader (make-reader inp fn))
                               (comment-tokens (reader-lookahead-comments reader inp)))
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
                                                     comment-tokens lib)))))))))))
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
                       (log/info "Refusing to symlink multi-form module "
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
  (define (skip-program? project artifact)
    (cond
      ((and (r7rs-program? artifact) (equal? (project-name project) ""))
       ;; R7RS programs can't be run in R6RS implementations unless
       ;; they are translated first.
       #f)
      ((not (artifact-for-bin? artifact))
       (log/trace "Skipping the program not for bin " (artifact-path artifact))
       #t)
      ((artifact-internal? artifact)
       (log/trace "Skipping the internal/private program " (artifact-path artifact))
       #t)
      (else #f)))
  (cond
    ((r6rs-library? artifact)
     (install-artifact/r6rs-library project artifact related-artifact* srcdir always-symlink?))
    ((or (r6rs-program? artifact) (r7rs-program? artifact))
     (cond
       ((skip-program? project artifact)
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
     ;; Install the native R7RS version and/or the R6RS translation.
     (let ((impl (artifact-implementation artifact))
           (same-path* (filter (lambda (art)
                                 (equal? (artifact-path art)
                                         (artifact-path artifact)))
                               related-artifact*)))
       (append
        (if (and (pair? same-path*) (eq? artifact (car same-path*)))
            (install-artifact/r7rs-library/native project artifact srcdir always-symlink?)
            '())                        ;already installed
        (install-artifact/r7rs-library project artifact srcdir always-symlink?))))
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
  (cond ((and (include-reference-realpath asset)
              (file-exists? (include-reference-realpath asset)))
         (let ((target (split-path (include-reference-path asset))))
           (list ((if always-symlink? symlink-file copy-file)
                  (path-join (libraries-directory) (car target))
                  (cdr target)
                  (include-reference-realpath asset)))))
        (else
         (log/debug "Not installing " (wrt (include-reference-path asset))
                    "; no source")
         '())))

;; Install a project and return a alist of artifact/asset => filename.
(define (install-project project always-symlink?)
  (let ((srcdir (project-source-directory project)))
    ;; Copy libraries, programs and assets to the file system. These
    ;; operations are ordered.
    (log/info "Installing " (if (equal? (project-name project) "")
                                "the current project"
                                (project-name project)))
    (cond
      ((equal? (project-installer project) '((r6rs)))
       (let* ((artifact* (find-artifacts srcdir #f))
              (artifact*
               (if (equal? (project-name project) "")
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

;; Installs activation scripts, like Python's virtualenv.
(define (install-activate-scripts)
  (mkdir/recursive (binaries-directory))
  (install-bash-activate-script)
  (install-fish-activate-script)
  (install-env-script))

(define (install-activate-script filename set set/export unset prepend prepend/export)
  (log/info "Installing " filename)
  (call-with-port (open-file-output-port filename
                                         (file-options no-fail)
                                         (buffer-mode block)
                                         (native-transcoder))
    (lambda (p)
      (let ((lib (path-join "$PWD" (libraries-directory)))
            (lib7 (path-join "$PWD" (r7rs-libraries-directory)))
            (ffi (path-join "$PWD" (ffi-libraries-directory)))
            (libobj (path-join "$PWD" (string-append (libraries-directory) "obj")))
            (bin (path-join "$PWD" (binaries-directory))))
        ;; This takes in RnRS_PATH from the existing environment and
        ;; prepends .akku/lib, and includes it in all the library path
        ;; variables, but it does not propagate the changed RnRS_PATH
        ;; to subshells.
        (fmt p
             (set "AKKU_CHEZ_PATH" "$R6RS_PATH") ;special for libobj
             (set "AKKU_R6RS_PATH" "$R6RS_PATH")
             (set "AKKU_R7RS_PATH" "$R7RS_PATH")
             (prepend "AKKU_CHEZ_PATH" (string-append lib "::" libobj))
             (prepend "AKKU_R6RS_PATH" lib)
             (prepend "AKKU_R7RS_PATH" lib7)
             ;; R6RS
             (set/export "CHEZSCHEMELIBDIRS" "$AKKU_CHEZ_PATH")
             (unset "CHEZSCHEMELIBEXTS")
             (set/export "GUILE_LOAD_PATH" "$AKKU_R6RS_PATH")
             (set/export "GUILE_LOAD_COMPILED_PATH" libobj)
             (set/export "IKARUS_LIBRARY_PATH" "$AKKU_R6RS_PATH")
             (set/export "MOSH_LOADPATH" "$AKKU_R6RS_PATH")
             (set/export "PLTCOLLECTS" ":$AKKU_R6RS_PATH")
             (set/export "SAGITTARIUS_LOADPATH" "$AKKU_R6RS_PATH")
             (set/export "VICARE_SOURCE_PATH" "$AKKU_R6RS_PATH")
             (set/export "YPSILON_SITELIB" "$AKKU_R6RS_PATH")
             (set/export "LARCENY_LIBPATH" "$AKKU_R6RS_PATH")
             (set/export "IRONSCHEME_LIBRARY_PATH" "$AKKU_R6RS_PATH") ;nonstandard
             (set/export "LOKO_LIBRARY_PATH" "$AKKU_R6RS_PATH")
             ;; R7RS
             (set/export "CHIBI_MODULE_PATH" "$AKKU_R7RS_PATH")
             (set/export "GAUCHE_LOAD_PATH" "$AKKU_R7RS_PATH")
             ;; For reaching programs installed from packages
             (prepend/export "PATH" bin)
             ;; For Linux
             (prepend/export "LD_LIBRARY_PATH" ffi)
             ;; For macOS
             (prepend/export "DYLD_LIBRARY_PATH" ffi)
             ;; Cleanup
             (unset "AKKU_CHEZ_PATH")
             (unset "AKKU_R6RS_PATH")
             (unset "AKKU_R7RS_PATH"))))))

(define (install-bash-activate-script)
  (define (set variable value)
    (cat variable "=" (wrt value) ";" nl))
  (define (set/export variable value)
    (cat "export " variable "=" (wrt value) ";" nl))
  (define (unset variable)
    (cat "unset " variable ";" nl))
  (define (prepend variable value)
    (cat variable "=" value
         "${" variable ":+:}" "$" variable ";" nl))
  (define (prepend/export variable value)
    (cat "export " variable "=" value
         "${" variable ":+:}" "$" variable ";" nl))
  (install-activate-script (path-join (binaries-directory) "activate")
                           set set/export unset prepend prepend/export))

(define (install-fish-activate-script)
  (define (set variable value)
    (cat "set " variable " " (wrt value) nl))
  (define (set/export variable value)
    (cat "set --export " variable " " (wrt value) nl))
  (define (unset variable)
    (cat "set --erase " variable nl))
  (define (prepend variable value)
    (cat "set --prepend " variable " " value nl))
  (define (prepend/export variable value)
    (cat "set --export --prepend " variable " " value nl))
  (install-activate-script (path-join (binaries-directory) "activate.fish")
                           set set/export unset prepend prepend/export))

(define (install-env-script)
  (let ((filename (path-join (akku-directory) "env")))
    (log/info "Installing " filename)
    (call-with-port (open-file-output-port filename
                                           (file-options no-fail)
                                           (buffer-mode block)
                                           (native-transcoder))
      (lambda (p)
        (fmt p
             "#!/bin/sh" nl
             "# Run this from anywhere to get a shell in the project environment  -*-sh-*-" nl
             "# To load in the current shell with bash: eval $(.akku/env -s)" nl
             "# For fish, use:                          .akku/env -f | source" nl
             "export AKKU_ENV=$(CDPATH='' cd -- \"$(dirname -- \"$0\")/..\" && pwd)" nl
             "dir=$(pwd)" nl
             "if [ ! -d \"$AKKU_ENV\" ] || [ ! -e \"$AKKU_ENV/.akku/bin/activate\" ]; then" nl
             "    echo The .akku/env script should be run, not sourced" nl
             "else" nl
             "    cd \"$AKKU_ENV\" || exit 1" nl
             "    . \"$AKKU_ENV/.akku/bin/activate\"" nl
             "" nl
             "    if [ \"$1\" = \"-s\" ]; then" nl
             "        echo \"AKKU_ENV=\\\"$AKKU_ENV\\\";\"" nl
             "        sed -e \"s/\\$PWD/\\$AKKU_ENV/g\" \"$AKKU_ENV/.akku/bin/activate\"" nl
             "        cd \"$dir\" || exit 1" nl
             "    elif [ \"$1\" = \"-f\" ]; then" nl
             "        echo \"set AKKU_ENV \\\"$AKKU_ENV\\\"\"" nl
             "        sed -e \"s/\\$PWD/\\$AKKU_ENV/g\" \"$AKKU_ENV/.akku/bin/activate.fish\"" nl
             "        cd \"$dir\" || exit 1" nl
             "    else" nl
             "        cd \"$dir\" || exit 1" nl
             "        SHELL=${SHELL:-/bin/sh}" nl
             "        exec \"${@:-$SHELL}\"" nl
             "    fi" nl
             "fi" nl)))
    (chmod filename #o755)))

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
              #f '((comment . "; This file is automatically generated and is not a copyrightable work.\n"))
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
                                      (if (equal? name "")
                                          "the current project"
                                          name))))
                            (log/info "The file " filename " in "
                                      (fmt-name (project-name other-project))
                                      " shadows the one from "
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

(define (post-install-checkups installed-project/artifact*)
  ;; Small advert for akku-r7rs if the user didn't install it but
  ;; there are R7RS libraries or programs installed.
  (when (and (not (exists (match-lambda
                           [#(project _artifact/fn*)
                            (equal? (project-name project) "akku-r7rs")])
                          installed-project/artifact*))
             (exists (match-lambda
                      [#(_project artifact/fn*)
                       (exists (match-lambda
                                [(artifact . fn*)
                                 (or (r7rs-library? artifact)
                                     (r7rs-program? artifact))])
                               artifact/fn*)])
                     installed-project/artifact*))
    (log/info "R7RS-small libraries are available in the akku-r7rs package")))

;; Install all projects, assuming that fetch already ran.
(define (install lockfile-location manifest-filename)
  (let ((project-list (read-lockfile lockfile-location))
        (current-project (make-dummy-project "" '(directory "."))))
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
      (remove-extraneous-files complete-list)
      (post-install-checkups complete-list))
    (install-activate-scripts))))
