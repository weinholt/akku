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
    (only (akku lib compat) system putenv)
    (akku lib git)
    (akku lib manifest)
    (akku lib utils))

(define (guess-public-git-location base-directory)
  (let lp ((remote* (git-list-remotes base-directory)))
    (if (null? remote*)
        #f
        (let ((url (git-remote-get-url base-directory (car remote*))))
          (cond
            ;; https://github.com/weinholt/akku.git
            ((regexp-matches (rx "https://" ($ (+ any))) url)
             => (lambda (m) url))
            ;; git@github.com:weinholt/akku.git
            ((regexp-matches (rx "git@" ($ (or "github.com" "gitlab.com"))
                                 ":" ($ (+ any))) url)
             => (lambda (m)
                  (string-append "https://" (regexp-match-submatch m 1) "/"
                                 (regexp-match-submatch m 2))))
            (else
             (lp (cdr remote*))))))))

;; Finds information to put into the lock part of the package index.
(define (get-git-lock base-directory version)
  (let ((version-tags (git-tag-list base-directory "v*"))
        (version-tag (string-append "v" version))
        (head-revision (git-rev-parse base-directory "HEAD"))
        (pull-url (guess-public-git-location base-directory)))
    (unless pull-url
      (error 'get-git-lock "The git location must be added to Akku.manifest"))
    (cond ((member version-tag version-tags)
           (let ((revision (git-rev-list/first base-directory version-tag)))
             (unless (string=? head-revision revision)
               (fmt (current-error-port)
                    ";; WARNING: Publishing tag that does not match the branch HEAD" nl))
             `((location (git ,pull-url))
               (tag ,version-tag)
               (revision ,revision))))
          (else
           (fmt (current-error-port) ";; INFO: Publishing untagged release" nl)
           `((location (git ,pull-url))
             (revision ,head-revision))))))

(define (package-filename pkg)
  (string-append (sanitized-name (package-name pkg)) "_"
                 (version-number (car (package-version* pkg))) ".akku"))

(define (gpg-detach-sign filename)
  (putenv "AKKU_FN" filename)
  (unless (and (zero? (system "set -x;gpg -sb \"$AKKU_FN\""))
               (file-exists? (string-append filename ".sig")))
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
           (fmt #t "Writing " fn " ..." nl)
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
       (fmt #t "Signing " fn " ..." nl)
       (gpg-detach-sign fn))
     package* filename*)
    ;; Submit.
    (for-each
     (lambda (archive-url)
       (fmt #t "Submitting to " archive-url " ..." nl)
       (putenv "AKKU_FN" (fmt #f (fmt-join
                                  (lambda (fn)
                                    (cat (dsp fn) (dsp ".sig")
                                         (dsp ",")
                                         (dsp fn)))
                                  filename* ",")))
       (putenv "AKKU_URL" (url-join archive-url "packages/"))
       (unless (zero? (system "set -x;curl --upload-file \"{$AKKU_FN}\" \"$AKKU_URL\""))
         (error 'submit "Error submitting" archive-url))
       archive-url*)
     archive-url*)))

(define (publish-packages manifest-filename base-directory archive-url* version-override)
  (cond
    ((file-exists? manifest-filename)
     (let ((package* (read-manifest manifest-filename #f version-override)))
       (when (null? package*)
         (error 'publish-packages "Empty manifest"))
       (when version-override
         (fmt #t ";; INFO: Version override: " version-override nl))
       (let* ((version (car (package-version* (car package*))))
              (lock (cond
                      ((is-git-repository? base-directory)
                       (get-git-lock base-directory (version-number version)))
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
     (fmt #t "Edit " manifest-filename " and run publish again" nl))))

)
