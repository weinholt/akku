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

;; Updating index files from a remote source.

(library (akku lib update)
  (export
    update-index)
  (import
    (rnrs (6))
    (only (srfi :1 lists) iota)
    (only (srfi :13 strings) string-prefix? string-suffix?)
    (only (srfi :67 compare-procedures) <? default-compare)
    (hashing sha-2)
    (industria openpgp)
    (semver versions)
    (wak fmt)
    (xitomatl alists)
    (xitomatl AS-match)
    (only (akku lib compat) directory-list rename-file pretty-print)
    (only (akku lib utils) print path-join url-join)
    (akku private http))

;; Verify the OpenPGP signature. The signature file can have multiple
;; signatures and only one valid signature is needed.
(define (verify-signature signed-filename signature-filename keys-directory keyfile-glob)
  (let ((keyfiles (filter (lambda (fn)
                            ;; TODO: Use keyfile-glob here to bind the
                            ;; repository index signature to a
                            ;; particular set of keys.
                            (string-suffix? ".gpg" fn))
                          (directory-list keys-directory))))
    (call-with-port (open-file-input-port signed-filename)
      (lambda (signed-port)
        (call-with-port (open-file-input-port signature-filename)
          (lambda (p)
            (let lp ()
              (let ((sig (get-openpgp-packet p)))
                (define (try-verify-with-keyfile keyfile)
                  (let ((keyring
                         (call-with-port (open-file-input-port (path-join keys-directory keyfile))
                           (lambda (p)
                             (get-openpgp-keyring/keyid p (openpgp-signature-issuer sig))))))
                    (set-port-position! signed-port 0)
                    (let-values (((result key) (verify-openpgp-signature sig keyring signed-port)))
                      (case result
                        ((missing-key)
                         (fmt #t ";; INFO: Signed by unknown key with ID "
                              (pad/left 16 (num key 16))))
                        (else
                         (fmt #t (if (eq? result 'good-signature) ";; INFO: Good"
                                     ";; WARNING: Bad")
                              " signature from "
                              (openpgp-format-fingerprint
                               (openpgp-public-key-fingerprint key))
                              " in " keyfile
                              nl)
                         (for-each
                          (lambda (data)
                            (when (openpgp-user-id? data)
                              (fmt #t ";; INFO: User ID: " (openpgp-user-id-value data) nl)))
                          (hashtable-ref keyring (openpgp-public-key-id key) #f))))
                      (and (eq? result 'good-signature) result))))
                (cond ((eof-object? sig) #f)
                      ((exists try-verify-with-keyfile keyfiles))
                      (else (lp)))))))))))

;; Download indices and merge them into one.
(define (update-index full-index-filename keys-directory repositories)
  (define (fetch-index suffix tag repository-url keyfile-glob)
    (let ((index-filename (string-append full-index-filename suffix))
          (sig-filename (string-append full-index-filename suffix ".sig"))
          (temp-filename (string-append full-index-filename suffix ".tmp"))
          (temp-sig-filename (string-append full-index-filename suffix ".tmp.sig"))
          (index-checksum (make-sha-256)))
      (when (file-exists? temp-filename)
        (delete-file temp-filename))
      (when (file-exists? temp-sig-filename)
        (delete-file temp-sig-filename))
      ;; Fetch the index to e.g. "index.db.0".
      (print ";; INFO: Fetching index from " repository-url " ...")
      (download-file (url-join repository-url "Akku-index.scm")
                     temp-filename
                     (lambda (buf) (sha-256-update! index-checksum buf)))
      ;; Download the signature by the SHA-256 of the file to be
      ;; signed. Prevents a race condition where the .sig file is
      ;; replaced during the previous download, and avoids using
      ;; cleartext signatures.
      (let ((index-sha256 (sha-256->string (sha-256-finish index-checksum))))
        (download-file (url-join repository-url
                                 (string-append "by-sha256/"
                                                (substring index-sha256 0 2)
                                                "/" index-sha256 ".sig"))
                       temp-sig-filename #f)
        ;; Verify the signature.
        (case (verify-signature temp-filename temp-sig-filename keys-directory keyfile-glob)
          ((good-signature)
           (rename-file temp-filename index-filename)
           (rename-file temp-sig-filename sig-filename)
           #t)
          (else #f)))))

  ;; Download indices.
  (for-each (lambda (i repo)
              (fetch-index (string-append "." (number->string i))
                           (assq-ref repo 'tag)
                           (assq-ref repo 'url)
                           (assq-ref repo 'keyfile)))
            (iota (length repositories)) repositories)

  ;; Merge indices.
  (let ((tempfile (string-append full-index-filename ".tmp")))
    (when (file-exists? tempfile)
      (delete-file tempfile))
    (call-with-output-file tempfile
      (lambda (outp)
        (define packages (make-hashtable equal-hash equal?))
        (display "#!r6rs ; -*- mode: scheme; coding: utf-8 -*-\n" outp)
        (display "(import (akku format index))\n" outp)
        ;; Gather versions from all indices.
        (for-each
         (lambda (i repo)
           (let ((tag (assq-ref repo 'tag)))
             (call-with-input-file (string-append full-index-filename "."
                                                  (number->string i))
               (lambda (inp)
                 (let lp ()
                   (match (read inp)
                     ((? eof-object?) #f)
                     (('package ('name name)
                                ('versions version* ...))
                      (let ((version-table
                             (cond ((hashtable-ref packages name #f))
                                   (else
                                    (let ((t (make-hashtable string-hash string=?)))
                                      (hashtable-set! packages name t)
                                      t)))))
                        (for-each
                         (lambda (version-spec)
                           (let ((version (car (assq-ref version-spec 'version))))
                             ;; The last repo to define a version,
                             ;; wins. TODO: Merge if there are
                             ;; multiple available download locations
                             ;; and otherwise the same hashes.
                             (hashtable-set! version-table version
                                             (cons (list 'repos (list tag)) version-spec))))
                         version*))
                      (lp))
                     (else (lp))))))))
         (iota (length repositories)) repositories)
        ;; Sort and print all versions.
        (let ((package-names (hashtable-keys packages)))
          (vector-sort! (lambda (x y) (<? default-compare x y)) package-names)
          (vector-for-each
           (lambda (package-name)
             (let* ((version-table (hashtable-ref packages package-name #f))
                    (semvers (vector-map string->semver (hashtable-keys version-table))))
               (vector-sort! (lambda (x y) (<? semver-compare x y)) semvers)
               (let ((versions (vector->list
                                (vector-map
                                 (lambda (semver)
                                   (hashtable-ref version-table (semver->string semver) #f))
                                 semvers))))
                 (pretty-print `(package (name ,package-name)
                                         (versions . ,versions))
                               outp))))
           package-names))))
    (rename-file tempfile full-index-filename)
    (fmt #t ";; INFO: Index updated" nl))))
