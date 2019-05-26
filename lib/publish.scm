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

;; Publish packages.

(library (akku lib publish)
  (export
    publish-packages)
  (import
    (rnrs (6))
    (semver versions)
    (srfi :115 regexp)
    (wak fmt)
    (wak fmt color)
    (only (xitomatl common) pretty-print)
    (only (akku private compat) putenv)
    (akku lib git)
    (akku lib manifest)
    (akku lib utils)
    (akku private logging))

(define logger:akku.http (make-logger logger:akku 'publish))
(define log/info (make-fmt-log logger:akku.http 'info))
(define log/error (make-fmt-log logger:akku.http 'error))
(define log/warn (make-fmt-log logger:akku.http 'warning))
(define log/debug (make-fmt-log logger:akku.http 'debug))
(define log/trace (make-fmt-log logger:akku.http 'trace))

(define (guess-public-git-location base-directory)
  (let lp ((remote* (git-list-remotes base-directory)))
    (if (null? remote*)
        #f
        (let ((url (git-remote-get-url base-directory (car remote*))))
          (cond
            ;; https://github.com/weinholt/akku.git
            ((regexp-matches (rx "https://" ($ (+ any))) url)
             => (lambda (m) url))
            ;; git://github.com/weinholt/akku.git
            ((regexp-matches (rx "git://" ($ (+ any))) url)
             => (lambda (m) url))
            ;; [ssh://]git@github.com[:/]weinholt/akku.git
            ((regexp-matches (rx (?? "ssh://") "git@" ($ (or "github.com" "gitlab.com"))
                                 (or ":" "/") ($ (+ any))) url)
             => (lambda (m)
                  (string-append "https://" (regexp-match-submatch m 1) "/"
                                 (regexp-match-submatch m 2))))
            (else
             (lp (cdr remote*))))))))

;; Finds information to put into the lock part of the package index.
(define (get-git-lock declared-location base-directory version tag-override)
  (let ((version-tags (git-tag-list base-directory "v*"))
        (version-tag (or tag-override (string-append "v" version)))
        (head-revision (git-rev-parse base-directory "HEAD"))
        (pull-url (guess-public-git-location base-directory)))
    (when (equal? head-revision "HEAD")
      (error 'get-git-lock "HEAD must point to a revision (please checkout a pushed branch)"))
    (unless (or declared-location pull-url)
      (error 'get-git-lock
             "Unrecognized remote; please add it to Akku.manifest as (location (git \"<pull-url>\"))"))
    (let ((location (or declared-location `(git ,pull-url))))
      (cond ((member version-tag version-tags)
             (let ((revision (git-rev-list/first base-directory version-tag)))
               (unless (string=? head-revision revision)
                 (log/warn "Publishing tag that does not match the branch HEAD"))
               `((location ,location)
                 (tag ,version-tag)
                 (revision ,revision))))
            (else
             (log/info "Publishing untagged release")
             `((location ,location)
               (revision ,head-revision)))))))

(define (package-filename pkg)
  (string-append (sanitized-name (package-name pkg)) "_"
                 (version-number (car (package-version* pkg))) ".akku"))

(define (gpg-detach-sign filename)
  (putenv "AKKU_FN" filename)
  (run-command "gpg -sb \"$AKKU_FN\"")
  (unless (file-exists? (string-append filename ".sig"))
    (error 'gpg-detach-sign "Could not sign with gpg" filename)))

(define (submit package* version base-directory archive-url*)
  ;; Create Akku index files.
  (let ((filename* (map package-filename package*)))
    (for-each
     (lambda (pkg fn)
       (if (file-exists? fn)
           (delete-file fn))
       (call-with-output-file fn
         (lambda (p)
           ;; This writes a fragment of an index file.
           (log/info "Writing " fn " ...")
           (fmt p "#!r6rs" nl)
           (for-each (lambda (archive-url)
                       (fmt p ";; Submit-To: " archive-url "packages/" nl))
                     archive-url*)
           (pretty-print (package->index-package pkg) p))))
     package* filename*)
    ;; Ocular inspection.
    (for-each
     (lambda (fn)
       (fmt #t (fmt-green (call-with-input-file fn get-string-all)) nl))
     filename*)
    (fmt #t "Submit the files shown above? (y/N) ")
    (flush-output-port (current-output-port))
    (unless (member (get-line (current-input-port)) '("yes" "y"))
      (error 'submit "User did not answer yes."))
    ;; Sign them.
    (for-each
     (lambda (pkg fn)
       (log/info "Signing " fn " ...")
       (gpg-detach-sign fn))
     package* filename*)
    ;; Submit.
    (for-each
     (lambda (archive-url)
       (log/info "Submitting to " archive-url " ...")
       (putenv "AKKU_FN" (fmt #f (fmt-join
                                  (lambda (fn)
                                    (cat (dsp fn) (dsp ".sig")
                                         (dsp ",")
                                         (dsp fn)))
                                  filename* ",")))
       (putenv "AKKU_URL" (url-join archive-url "packages/"))
       (run-command "set -x;curl --upload-file \"{$AKKU_FN}\" \"$AKKU_URL\"")
       archive-url*)
     archive-url*)))

(define (publish-packages manifest-filename base-directory archive-url*
                          version-override tag-override)
  (cond
    ((file-exists? manifest-filename)
     (let ((package* (read-manifest manifest-filename #f version-override)))
       (when (null? package*)
         (error 'publish-packages "Empty manifest"))
       (when version-override
         (log/info "Version override: " version-override))
       (when tag-override
         (log/info "Tag override: " tag-override))
       (let* ((version (car (package-version* (car package*))))
              (declared-location (cond
                                   ((assq 'location (version-lock version))  =>
                                    cadr)
                                   (else #f)))
              (lock (cond
                      ((is-git-repository? base-directory)
                       (get-git-lock declared-location base-directory (version-number version) tag-override))
                      (else
                       (error 'publish-packages
                              "Publishing from non-git repositories is not yet supported")))))
         ;; TODO: each package* needs a separate lock if uploaded as tarballs
         (version-lock-set! version lock)
         (submit package* version base-directory archive-url*))))
    (else
     (write-manifest manifest-filename
                     (list (draft-akku-package version-override ;XXX: use highest version tag
                                               `(location (git "https://example.com/")))))
     (log/error "Edit " manifest-filename " and run publish again")))))
