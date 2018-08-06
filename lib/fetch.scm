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

;; Fetches projects specified in the lockfile to .akku/src.

(library (akku lib fetch)
  (export
    fetch
    project-source-directory
    logger:akku.fetch)
  (import
    (rnrs (6))
    (prefix (compression tar) tar:)
    (compression gzip)
    (compression xz)
    (hashing sha-2)
    (only (spells filesys) file-directory? file-symbolic-link? rename-file)
    (wak fmt)
    (xitomatl AS-match)
    (akku lib git)
    (akku lib lock)
    (akku lib utils)
    (akku private http)
    (akku private logging))

(define logger:akku.fetch (make-logger logger:akku 'fetch))
(define log/info (make-fmt-log logger:akku.fetch 'info))
(define log/warn (make-fmt-log logger:akku.fetch 'warning))
(define log/error (make-fmt-log logger:akku.fetch 'error))
(define log/debug (make-fmt-log logger:akku.fetch 'debug))
(define log/trace (make-fmt-log logger:akku.fetch 'trace))

(define (akku-directory)                ;XXX: where to put this?
  ".akku")

(define (sources-directory*) "src")

(define (sources-directory)
  (path-join (akku-directory) (sources-directory*)))

(define (project-source-directory project)
  (match (project-source project)
    (('directory dir)
     ;; For sources in directories, refer directly to that directory.
     dir)
    (else
     ;; Otherwise a local src directory must be created.
     (path-join (sources-directory) (project-sanitized-name project)))))

(define (project-cache-file project)
  (path-join (cache-directory)
             (string-append (project-sanitized-name project)
                            "-"
                            (cadr (assq 'sha256 (project-content project))))))

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
      (let ((result (if (string-ci=? expected-digest
                                     computed-digest)
                        'ok 'bad)))
        (log/debug "verify-file-contents"
                   " filename=" filename
                   " expected=" expected-digest " computed=" computed-digest
                   " result=" result)
        result))))

;; Extracts a tarball into a directory.
(define (extract-tarball tarball directory)
  (call-with-port (cond ((is-gzip-file? tarball)
                         (log/trace "Detected gzip: " (wrt tarball))
                         (open-gzip-file-input-port tarball))
                        ((is-xz-file? tarball)
                         (log/trace "Detected xz: " (wrt tarball))
                         (open-xz-file-input-port tarball))
                        (else
                         (error 'extract-tarball
                                "Unsupported compression method" tarball directory)))
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
                   (eq? 'ok (verify-file-contents cached-file (project-content project))))
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

;; Fetch all projects in the lockfile.
(define (fetch lockfile-location)
  (log/trace "Fetching projects for " (wrt lockfile-location))
  (let ((project-list (read-lockfile lockfile-location)))
    (mkdir/recursive (akku-directory))
    (let ((gitignore (path-join (akku-directory) ".gitignore")))
      (unless (file-exists? gitignore)
        (call-with-output-file gitignore
          (lambda (p)
            (display (sources-directory*) p)
            (newline p)))))
    (for-each fetch-project project-list))))
