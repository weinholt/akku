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

;; Lock file creation.

(library (akku lib lock)
  (export
    add-dependency
    remove-dependencies
    lock-dependencies
    list-packages
    logger:akku.lock)
  (import
    (rnrs (6))
    (only (srfi :1 lists) iota append-map filter-map)
    (only (srfi :67 compare-procedures) <? default-compare)
    (semver versions)
    (semver ranges)
    (spdx parser)
    (spells logging)
    (wak fmt)
    (wak fmt color)
    (xitomatl alists)
    (xitomatl AS-match)
    (only (akku lib compat) pretty-print rename-file getcwd)
    (akku lib manifest)
    (akku lib solver)
    (akku lib solver choice)
    (akku lib solver dummy-db)          ;TODO: Make a proper database
    (only (akku lib solver internals) make-universe)
    (only (akku lib solver logging) dsp-universe)
    (only (akku private utils) make-fmt-log logger:akku)
    (only (akku lib utils) split-path get-terminal-size)
    (prefix (akku lib solver universe) universe-))

(define logger:akku.lock (make-logger logger:akku 'lock))
(define log/info (make-fmt-log logger:akku.lock 'info))
(define log/warn (make-fmt-log logger:akku.lock 'warning))
(define log/debug (make-fmt-log logger:akku.lock 'debug))
(define log/trace (make-fmt-log logger:akku.lock 'trace))

(define (read-package-index index-filename manifest-packages)
  (let ((db (make-dummy-db))
        (packages (make-hashtable equal-hash equal?)))
    ;; Add the packages from the manifest.
    (for-each (lambda (pkg)
                (dummy-db-add-package! db (package-name pkg) (list 0) 0)
                (hashtable-set! packages (package-name pkg) pkg))
              manifest-packages)
    ;; Read packages from the index.
    (call-with-input-file index-filename
      (lambda (p)
        (let lp ()
          (match (read p)
            ((? eof-object?) #f)
            (('package ('name name)
                       ('versions version* ...))
             ;; XXX: Versions must be semver-sorted in ascending
             ;; order.
             (dummy-db-add-package! db name (cons #f (iota (length version*))) #f)
             (hashtable-set! packages name
                             (make-package name (map parse-version version*)))
             (lp))
            (else (lp))))))             ;allow for future expansion
    (values db packages)))

;; Get scores and choices for the packages in the manifest. These are
;; scored very high and set to already be installed.
(define (scores/choices db manifest-packages)
  (let lp ((manifest-packages manifest-packages)
           (version-scores '())
           (initial-choices (make-choice-set)))
    (cond
      ((null? manifest-packages)
       (values version-scores initial-choices))
      (else
       (let* ((pkg (car manifest-packages))
              (pkg-name (package-name pkg)))
         (lp (cdr manifest-packages)
             (cons (cons (dummy-db-version-ref db pkg-name 0) 10000)
                   version-scores)
             (choice-set-insert-or-narrow
              initial-choices
              (make-install-choice (dummy-db-version-ref db pkg-name 0) 0))))))))

;; Takes two choice-sets containing chosen packages and returns a list
;; of projects for a lockfile.
(define (choice-set->project-list packages manifest-packages
                                  initial-choices choices-in-solution)
  (define (choice->project choice)
    (let* ((chosen-tag (universe-version-tag (choice-version choice)))
           (pkg (universe-version-package (choice-version choice)))
           (name (universe-package-name pkg))
           (requested-version (choice-set-version-of initial-choices pkg))
           (current-tag (universe-version-tag (universe-package-current-version pkg))))
      (log/debug "Project " name " v" chosen-tag " (was v" current-tag ")")
      (cond ((and requested-version
                  (universe-version-tag requested-version)
                  (not chosen-tag))
             ;; A package from the manifest was not chosen.
             #f)
            ((memp (lambda (pkg) (equal? (package-name pkg) name))
                   manifest-packages)
             ;; Don't return a project for packages in the manifest.
             'in-manifest)
            ((not chosen-tag)
             `((name ,name) (no project chosen!)))
            (else
             ;; This goes into the lockfile.
             (let* ((pkg (hashtable-ref packages name #f))
                    (ver (list-ref (package-version* pkg) chosen-tag)))
               (log/info "Locked " name " v" (version-number ver))
               `((name ,name)
                 ,@(version-lock ver)))))))
  (choice-set-fold (lambda (choice acc)
                     (let ((project (choice->project choice)))
                       (if (eq? project 'in-manifest)
                           acc
                           (cons project acc))))
                   '()
                   (choice-set-union initial-choices choices-in-solution)))

(define (dependencies->version-tags packages pkg lst)
  (let lp ((lst lst))
    (match lst
      [('or pkg* ...)
       (append-map lp pkg*)]
      [(name (? string? range))
       ;; TODO: Don't crash when the depended-on package doesn't
       ;; exist.
       (let ((package (hashtable-ref packages name #f)))
         (unless package
           (error 'dependencies->version-tags "No such package in the index" name))
         (let* ((available-version* (package-version* package))
                (m (semver-range->matcher range))
                (tag* (filter-map
                       (lambda (tag pkgver)
                         (and (m (version-semver pkgver)) tag))
                       (iota (length available-version*))
                       available-version*)))
           (when (null? tag*)
             ;; TODO: Don't crash when no versions are in the range.
             (error 'dependencies->version-tags "No matching versions"
                    (package-name pkg)
                    name
                    (semver-range->string (semver-range-desugar (string->semver-range range)))
                    (map version-number available-version*)))
           ;; To satisfy the dependency, any of these (name . tag) pairs
           ;; can be used.
           (map (lambda (tag) (cons name tag)) tag*)))])))

;; Adds dependencies between packages.
(define (add-package-dependencies db packages manifest-packages dev-mode?)
  (define (process-package-version pkg version-idx version)
    (define (process-deps lst conflict?)
      (log/debug "dependency: " (package-name pkg) " " (version-number version) " "
                 (if conflict? "conflicts" "depends") " " lst)
      (let ((deps (dependencies->version-tags packages pkg lst)))
        (unless (null? deps)
          (dummy-db-add-dependency! db (package-name pkg) version-idx conflict?
                                    deps))))
    (for-each (lambda (dep) (process-deps dep #f))
              (version-depends version))
    (for-each (lambda (dep) (process-deps dep #t))
              (version-conflicts version))
    (when (and dev-mode? (memq pkg manifest-packages))
      ;; Dev mode: add dev dependencies for packages in the manifest.
      (for-each (lambda (dep) (process-deps dep #f))
                (version-depends/dev version))))
  (let-values (((pkg-names pkgs) (hashtable-entries packages)))
      (vector-for-each
       (lambda (name pkg)
         (log/debug "package " name " has versions "
                    (map version-number (package-version* pkg)))
         (for-each (lambda (version-idx version)
                     (log/debug "processing " name ": " version)
                     (process-package-version pkg version-idx version))
                   (iota (length (package-version* pkg)))
                   (package-version* pkg)))
       pkg-names pkgs)))

;; Write the lockfile.
(define (write-lockfile lockfile-filename projects dry-run?)
  (call-with-port (if dry-run?
                      (current-output-port)
                      (open-file-output-port
                       (string-append lockfile-filename ".tmp")
                       (file-options no-fail)
                       (buffer-mode block)
                       (native-transcoder)))
    (lambda (p)
      (display "#!r6rs ; -*- mode: scheme; coding: utf-8 -*-\n" p)
      (display ";; This file is automatically generated - do not change it by hand.\n" p)
      (pretty-print `(import (akku format lockfile)) p)
      (pretty-print `(projects ,@projects) p)))
  (rename-file (string-append lockfile-filename ".tmp") lockfile-filename)
  (log/info "Wrote " lockfile-filename))

(define (lock-dependencies manifest-filename lockfile-filename index-filename)
  (define dry-run? #f)
  (define dev-mode? #t)
  (define manifest-packages (read-manifest manifest-filename 'mangle-names #f))

  (let-values (((db packages) (read-package-index index-filename manifest-packages)))
    (add-package-dependencies db packages manifest-packages dev-mode?)
    (let-values (((version-scores initial-choices) (scores/choices db manifest-packages)))
      (let* ((universe (dummy-db->universe db))
             (solver (make-solver universe
                                  `((version-scores . ,version-scores)
                                    (initial-choices . ,initial-choices)))))
        (log/debug (dsp-universe universe))
        (let lp ()
          (let ((solution (find-next-solution! solver 10000)))
            (cond
              (solution
               (let ((projects
                      (choice-set->project-list packages
                                                manifest-packages
                                                initial-choices
                                                (solution-choices solution))))
                 (cond ((not (exists not projects))
                        (write-lockfile lockfile-filename projects dry-run?))
                       (else
                        ;; TODO: log what is bad about this solution.
                        (log/info "Rejected solution, trying the next...")
                        (lp)))))
              (else
               (error 'lock-dependencies "No acceptable solution - dependency hell")))))))))

(define (update-manifest manifest-filename proc)
  (let ((akku-package*
         (call-with-input-file manifest-filename
           (lambda (p)
             (let lp ((pkg* '()))
               (match (read p)
                 ((and ('akku-package (_ _) . _) akku-package)
                  (cons (proc akku-package) pkg*))
                 ((? eof-object?) pkg*)
                 (else (lp pkg*))))))))
    (write-manifest manifest-filename (reverse akku-package*))
    (log/info "Wrote " manifest-filename)))

;; Adds a dependency to the manifest. FIXME: needs to be moved to
;; somewhere else.
(define (add-dependency manifest-filename index-filename dev? dep-name dep-range)
  (define manifest-packages
    (if (file-exists? manifest-filename)
        (read-manifest manifest-filename #f #f)
        '()))
  (define (get-suitable-range version*)
    ;; TODO: This might pick a range that is not installable together
    ;; with the rest of the currently locked packages.
    (let ((semver* (map version-semver version*)))
      (let lp ((semver* semver*) (highest (car semver*)))
        (cond ((null? semver*)
               ;; This range picks something that will stay
               ;; compatible.
               (string-append "^" (semver->string highest)))
              ((and (<? semver-compare highest (car semver*))
                    ;; If highest is stable, then don't select a
                    ;; pre-release.
                    (not (and (null? (semver-pre-release-ids highest))
                              (not (null? (semver-pre-release-ids (car semver*)))))))
               (lp (cdr semver*) (car semver*)))
              (else
               (lp (cdr semver*) highest))))))
  (let-values (((_ packages) (read-package-index index-filename manifest-packages)))
    (cond
      ((hashtable-ref packages dep-name #f)
       => (lambda (package)
            (let ((package-name (package-name package))
                  (range (or dep-range (get-suitable-range (package-version* package)))))
              (log/info "Adding " package-name "@" range " to " manifest-filename "...")
              (cond ((file-exists? manifest-filename)
                     (update-manifest
                      manifest-filename
                      (match-lambda
                       (('akku-package (name version) prop* ...)
                        `(akku-package
                          (,name ,version)
                          ,@(assq-update prop*
                                         (if dev? 'depends/dev 'depends)
                                         (lambda (prev)
                                           (assoc-replace prev package-name
                                                          (list range)))
                                         '()))))))
                    (else
                     ;; XXX: This is a manifest that can actually be
                     ;; used immediately, unlike the one in init.
                     (write-manifest
                      manifest-filename
                      (list (draft-akku-package #f
                                                `(,(if dev? 'depends/dev 'depends)
                                                  (,package-name ,range)))))
                     (log/info "Created a draft manifest in " manifest-filename))))))
      (else
       (error 'add-dependency "Package not found" dep-name)))))

(define (remove-dependencies manifest-filename dep-name*)
  (cond ((file-exists? manifest-filename)
         (update-manifest
          manifest-filename
          (match-lambda
           (('akku-package (name version) prop* ...)
            `(akku-package
              (,name ,version)
              ,@(map
                 (match-lambda
                  [((and (or 'depends 'depends/dev) dep-type) . dep-list)
                   (cons dep-type
                         (remp (match-lambda
                                [(pkg-name range)
                                 (let ((do-remove (member pkg-name dep-name*)))
                                   (when do-remove
                                     (log/info "Removing " pkg-name "@" range " from "
                                               manifest-filename "..."))
                                   do-remove)])
                               dep-list))]
                  [x x])
                 prop*))))))
        (else
         (log/warn "No manifest: nothing to do"))))

;; Lists packages in the index.
(define (list-packages manifest-filename lockfile-filename index-filename)
  (define manifest-packages
    (if (file-exists? manifest-filename)
        (read-manifest manifest-filename)
        '()))
  (define lock-spec*         ;FIXME: move to a common parser lib
    (if (file-exists? lockfile-filename)
        (call-with-input-file lockfile-filename
          (lambda (p)
            (let lp ((ret '()))
              (match (read p)
                ((? eof-object?) ret)
                (('projects . prj*)
                 (lp (append (map (match-lambda
                                   [(('name name) . lock-spec)
                                    lock-spec])
                                  prj*)
                             ret)))
                (_ (lp ret))))))
        '()))
  (define deps                          ;package name -> (type . range)
    (let ((deps (make-hashtable equal-hash equal?)))
      (for-each
       (lambda (pkg)
         (define (set-ranges type depends)
           (for-each
            (match-lambda
             [(package-name range)
              (hashtable-set! deps package-name
                              (cons type (semver-range->matcher range)))])
            depends))
         (let ((v (car (package-version* pkg)))) ;manifest has a single version
           (set-ranges 'depends (version-depends v))
           (set-ranges 'depends/dev (version-depends/dev v))))
       manifest-packages)
      deps))
  (let-values (((_ packages) (read-package-index index-filename '()))
               ((terminal-cols _terminal-lines) (get-terminal-size)))
    (fmt #t ",-- (L) The version is in the lockfile" nl
            "|,- (M) The version matches the range in the manifest / (D) Dev. dependency" nl)
    (fmt #t "||" (space-to 3) "Package name" (space-to 20) "SemVer" (space-to 36) "Synopsis"
         nl
         (pad-char #\= (space-to (max 1 (- terminal-cols 1))))
         nl)
    (let ((package-names (hashtable-keys packages)))
      (vector-sort! (lambda (x y) (<? default-compare x y)) package-names)
      (vector-for-each
       (lambda (package-name)
         (let ((package (hashtable-ref packages package-name #f)))
           (for-each
            (lambda (version)
              (let ((version-locked? (member (version-lock version) lock-spec*))
                    (manifest-match (cond ((hashtable-ref deps package-name #f)
                                           => (match-lambda
                                               ((type . range-matcher)
                                                (if (range-matcher (version-semver version))
                                                    (if (eq? type 'depends) "M" "D")
                                                    #f))))
                                          (else #f))))
                (let ((colorize (cond ((and version-locked? manifest-match)
                                       (lambda (x) (fmt-bold (fmt-green x))))
                                      (manifest-match fmt-green)
                                      (version-locked? fmt-cyan)
                                      (else (lambda (x) x)))))
                  (fmt #t
                       (if version-locked? "L" "")
                       (space-to 1)
                       (or manifest-match "")
                       (space-to 3) package-name
                       (space-to 20)
                       (ellipses "…"
                                 (colorize (pad 16 (trim 15 (version-number version))))
                                 (trim (max 10 (- terminal-cols 37))
                                       (cond ((version-synopsis version) => car)
                                             (else "-"))))
                       nl))))
            (package-version* package))))
       package-names)))))
