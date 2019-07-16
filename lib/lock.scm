;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2018, 2019 Göran Weinholt <goran@weinholt.se>
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
    show-package

    ;; Lockfile parsing
    read-lockfile
    make-project make-dummy-project project?
    project-name
    project-packages
    project-source
    project-installer
    project-tag
    project-revision
    project-content
    project-sanitized-name
    project-scripts
    project-locks-package-version?

    ;; Logging
    logger:akku.lock)
  (import
    (rnrs (6))
    (only (srfi :1 lists) last iota append-map filter-map list-index)
    (only (srfi :13 strings) string-prefix?)
    (only (srfi :67 compare-procedures) <? string-compare-ci)
    (semver versions)
    (semver ranges)
    (only (spells filesys) rename-file)
    (spdx parser)
    (wak fmt)
    (wak fmt color)
    (chibi match)
    (akku lib manifest)
    (akku lib solver)
    (akku lib solver choice)
    (akku lib solver dummy-db)          ;TODO: Make a proper database
    (only (akku lib solver internals) make-universe)
    (only (akku lib solver logging) dsp-universe)
    (only (akku lib utils) split-path get-terminal-size sanitized-name
          assq-ref assoc-replace assq-update)
    (prefix (akku lib solver universe) universe-)
    (only (akku private compat) pretty-print)
    (akku private logging))

(define logger:akku.lock (make-logger logger:akku 'lock))
(define log/info (make-fmt-log logger:akku.lock 'info))
(define log/warn (make-fmt-log logger:akku.lock 'warning))
(define log/debug (make-fmt-log logger:akku.lock 'debug))
(define log/trace (make-fmt-log logger:akku.lock 'trace))

(define-record-type project
  (fields name packages source
          installer                     ;for future extensions
          scripts
          ;; one of these:
          tag revision content)
  (sealed #t))

;; Turns a project name into something that works as a directory name.
(define (project-sanitized-name project)
  (sanitized-name (project-name project)))

(define (lock-key? x)
  (memq x '(install installer location tag revision content)))

(define (assq-car-ref alist key def)
  (cond ((assq-ref alist key #f) => car)
        (else def)))

(define (parse-project spec)
  (let ((name (car (assq-ref spec 'name)))
        (tag (cond ((assq 'tag spec) => cadr) (else #f)))
        (revision (cond ((assq-ref spec 'revision #f) => car) (else #f)))
        (location (assq 'location spec))
        (content (assq-ref spec 'content #f)))
    (assert (or (list? name) (not (char=? (string-ref name 0) #\())))
    (make-project name
                  (cond ((assq 'install spec) => cdr) (else #f))
                  (car (assq-ref spec 'location))
                  (assq-ref spec 'installer '((r6rs)))
                  (assq-ref spec 'scripts '())
                  tag revision content)))

;; If the installer is given `project`, does that mean it will install
;; `version`?
(define (project-locks-package-version? project version)
  (let ((ver-lock (version-lock version)))
    (and
      (equal? (project-source project) (assq-car-ref ver-lock 'location #f))
      (equal? (project-tag project) (assq-car-ref ver-lock 'tag #f))
      (equal? (project-revision project) (assq-car-ref ver-lock 'revision #f))
      (equal? (project-content project) (assq-ref ver-lock 'content #f)))))

(define (make-dummy-project name location)
  (make-project name #f location '((r6rs)) '() #f #f #f))

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

;; Direct dependencies are in the manifest and replace whatever
;; information there might be about them in the index. The info in the
;; direct dependency is used to create the lock without consulting the
;; index.
(define (parse-direct-dependencies manifest-packages)
  (define who 'parse-direct-dependencies)
  (define (direct-deps-from-pkg pkg)
    (define (filter-direct-dep dep)
      ;; Look at a dependency of a package in the manifest and see if
      ;; it contains a direct dependency on a project in git. If so,
      ;; return a package with info from that dependency as a version
      ;; record.
      (match dep
        [('or pkg* ...)
         (append-map filter-direct-dep pkg*)]
        [(_name (? string? _version))
         '()]
        [(name (? list? prop*) ...)
         (let ((maybe-version (assq-ref prop* 'version #f))
               (maybe-location (assq-ref prop* 'location #f))
               (maybe-content (assq-ref prop* 'content #f))
               (maybe-revision (assq-ref prop* 'revision #f))
               (maybe-tag (assq-ref prop* 'tag #f)))
           (unless (and maybe-version
                        maybe-location
                        (or (match maybe-location
                              [(('directory _)) #t]
                              [else #f])
                            maybe-revision maybe-content))
             (assertion-violation who
                                  "Direct dependencies require: version, location and revision or content"
                                  name prop*))
           (let ((version (car maybe-version))
                 (location (car maybe-location)))
             (let ((version-spec `((version ,version)
                                   (lock (location ,location)
                                         ,@(if maybe-revision `((revision ,@maybe-revision)) '())
                                         ,@(if maybe-content `((content ,@maybe-content)) '())
                                         ,@(if maybe-tag `((tag ,@maybe-tag)) '())))))
               (list (make-package name (list (parse-version version-spec)))))))]))
    (append-map (lambda (version)
                  (append
                   (append-map filter-direct-dep (version-depends version))
                   (append-map filter-direct-dep (version-depends/dev version))))
                (package-version* pkg)))
  (append-map direct-deps-from-pkg manifest-packages))

(define (read-package-index index-filename manifest-packages)
  (let ((db (make-dummy-db))
        (packages (make-hashtable equal-hash equal?))
        (direct-dependencies (parse-direct-dependencies manifest-packages)))
    (log/trace "Found direct dependencies: " direct-dependencies)
    ;; Add the packages from the manifest and direct dependencies.
    (for-each (lambda (pkg)
                (if (memq pkg manifest-packages)
                    (dummy-db-add-package! db (package-name pkg) (list 0) 0)
                    (dummy-db-add-package! db (package-name pkg) (list #f 0) #f))
                (when (hashtable-contains? packages (package-name pkg))
                  (assertion-violation 'read-package-index
                                       "Duplicate package in the manifest"
                                       (package-name pkg)))
                (hashtable-set! packages (package-name pkg) pkg))
              (append manifest-packages direct-dependencies))
    ;; Read packages from the index.
    (call-with-input-file index-filename
      (lambda (p)
        (let lp ()
          (match (read p)
            ((? eof-object?) #f)
            (('package ('name name)
                       ('versions version* ...))
             ;; XXX: Versions must be semver-sorted in ascending
             ;; order. This is arranged by the archive software.
             (cond ((hashtable-ref packages name #f)
                    (log/debug "Ignoring " name " from the index because it is in the manifest"))
                   (else
                    (dummy-db-add-package! db name (cons #f (iota (length version*))) #f)
                    (hashtable-set! packages name
                                    (make-package name (map parse-version version*)))))
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
                 ,@(version-lock ver)
                 ,@(if (null? (version-scripts ver))
                       '()
                       `((scripts ,@(version-scripts ver))))))))))
  (choice-set-fold (lambda (choice acc)
                     (let ((project (choice->project choice)))
                       (if (eq? project 'in-manifest)
                           acc
                           (cons project acc))))
                   '()
                   (choice-set-union initial-choices choices-in-solution)))

;; Given a package and a dependency *dep-spec* this returns a list of
;; (name . tag) pairs showing which versions satisfy the dependencies
;; (or conflicts). If the dependencies are unsatisfiable then this
;; returns #f. This case is distinguished
(define (dependencies->version-tags packages pkg dep-spec is-essential?)
  (let lp ((dep-spec dep-spec))
    (match dep-spec
      [('or pkg* ...)
       (append-map lp pkg*)]
      [(name (? string? range))
       (cond
         ((hashtable-ref packages name #f) =>
          (lambda (package)
            (let* ((available-version* (package-version* package))
                   (m (semver-range->matcher range))
                   (tag* (filter-map
                          (lambda (tag pkgver)
                            (and (m (version-semver pkgver)) tag))
                          (iota (length available-version*))
                          available-version*)))
              (cond ((and (null? tag*) is-essential?)
                     (assertion-violation 'dependencies->version-tags
                                          "No matching versions"
                                          (package-name pkg)
                                          name
                                          (semver-range->string
                                           (semver-range-desugar (string->semver-range range)))
                                          (map version-number available-version*)))
                    ((and (null? tag*) (not is-essential?))
                     (log/debug "Dependency " (package-name pkg) " -> "
                                name "@" range " is unsatisfiable")
                     '())
                    (else
                     ;; To satisfy the dependency, any of these (name . tag) pairs
                     ;; can be used.
                     (log/trace "Depends for " (wrt name) " match " tag*)
                     (map (lambda (tag) (cons name tag)) tag*))))))
         (else
          (log/warn "Unknown package: " (wrt name))
          '()))]
      [(name (? list? prop*) ...)
       (log/trace "Tagging up " (wrt name) " with props " (wrt prop*))
       (lp (list name (cond ((assq-ref prop* 'version) => car)
                            (else
                             (assertion-violation 'dependencies->version-tags
                                                  "A version is required here"
                                                  name prop*)))))])))

;; Adds dependencies between packages.
(define (add-package-dependencies db packages manifest-packages dev-mode?)
  (define (process-package-version pkg version-idx version)
    (define (process-deps dep-spec conflict?)
      (log/debug "dependency: " (package-name pkg) " " (version-number version) " "
                 (if conflict? "conflicts" "depends") " " dep-spec)
      (let ((deps (dependencies->version-tags packages pkg dep-spec (memq pkg manifest-packages))))
        (unless (null? dep-spec)
          (dummy-db-add-dependency! db (package-name pkg) version-idx conflict?
                                    (or deps '())))))
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

;; Given a package, return just the names of the dependencies.
(define (package->dependency-package-names package)
  (append-map
   (lambda (ver)
     (append-map
      (lambda (dep-spec)
        (let lp ((dep-spec dep-spec))
          (match dep-spec
            [('or pkg* ...)
             (append-map lp pkg*)]
            [(name (? string? range))
             (list name)]
            [(name (? list? prop*) ...)
             (list name)])))
      (version-depends ver)))
   (package-version* package)))

;; Reorder the project list. First install projects which are not
;; explicitly listed in the manifest, preserving the order used by the
;; solver. Then install projects mentioned in the manifest, using the
;; order given in the manifest. (The current project is implictly
;; installed last).
(define (reorder-project-list projects manifest-packages)
  (define dep-names (append-map package->dependency-package-names
                                manifest-packages))
  (let-values ([(explicit implicit)
                (partition
                 (lambda (project)
                   (member (project-name (parse-project project))
                           dep-names))
                 projects)])
    (append implicit
            (list-sort (lambda (p0 p1)
                         (let ((name0 (project-name (parse-project p0)))
                               (name1 (project-name (parse-project p1))))
                           (< (list-index (lambda (x) (equal? x name0)) dep-names)
                              (list-index (lambda (x) (equal? x name1)) dep-names))))
                       explicit))))

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
  (define manifest-packages
    (if (file-exists? manifest-filename)
        (read-manifest manifest-filename 'mangle-names #f)
        '()))
  (let-values (((db packages) (read-package-index index-filename manifest-packages)))
    (add-package-dependencies db packages manifest-packages dev-mode?)
    (let-values (((version-scores initial-choices) (scores/choices db manifest-packages)))
      (let* ((universe (dummy-db->universe db))
             (solver (make-solver universe
                                  `((version-scores . ,version-scores)
                                    (initial-choices . ,initial-choices)))))
        (log/debug (dsp-universe universe))
        (log/info "Solving dependencies...")
        (let lp ()
          (let ((solution (find-next-solution! solver 10000)))
            (cond
              (solution
               (let* ((projects
                       (choice-set->project-list packages
                                                 manifest-packages
                                                 initial-choices
                                                 (solution-choices solution)))
                      (projects (reorder-project-list projects manifest-packages)))
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
         (if (file-exists? manifest-filename)
             (call-with-input-file manifest-filename
               (lambda (p)
                 (let lp ((pkg* '()))
                   (match (read p)
                     ((and ('akku-package (_ _) . _) akku-package)
                      (cons (proc akku-package) pkg*))
                     ((? eof-object?) pkg*)
                     (else (lp pkg*))))))
             '())))
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
                                [(pkg-name attr* ...)
                                 (let ((do-remove (member pkg-name dep-name*)))
                                   (when do-remove
                                     (match attr*
                                       [((? string? range))
                                        (log/info "Removing " pkg-name "@" range " from "
                                                  manifest-filename "...")]
                                       [else
                                        (log/info "Removing " pkg-name " from "
                                                  manifest-filename "...")]))
                                   do-remove)])
                               dep-list))]
                  [x x])
                 prop*))))))
        (else
         (log/warn "No manifest: nothing to do"))))

;; Lists packages in the index.
(define (list-packages manifest-filename lockfile-filename index-filename)
  (define (compare-names x y)
    (define (strip-paren x)
      (if (string? x)
          x
          (let ((str
                 (call-with-string-output-port
                   (lambda (p)
                     (display x p)))))
            (substring str 1 (- (string-length str) 1)))))
    (string-compare-ci (strip-paren x) (strip-paren y)))
  (define manifest-packages
    (if (file-exists? manifest-filename)
        (read-manifest manifest-filename)
        '()))
  (define lockfile-projects
    (if (file-exists? lockfile-filename)
        (read-lockfile lockfile-filename)
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
                              (cons type (semver-range->matcher range)))]
             [(name (? list? prop*) ...)
              (set-ranges type (list (list name (car (assq-ref prop* 'version)))))])
            depends))
         (let ((v (car (package-version* pkg)))) ;manifest has a single version
           (set-ranges 'depends (version-depends v))
           (set-ranges 'depends/dev (version-depends/dev v))))
       manifest-packages)
      deps))
  (let-values (((_ packages) (read-package-index index-filename '()))
               ((terminal-cols _terminal-lines) (get-terminal-size)))
    (let ((package-names (hashtable-keys packages))
          (vercol 24))                  ;version column
      (fmt #t ",-- (L) The version is in the lockfile" nl
           "|,- (M) The version matches the range in the manifest / (D) Dev. dependency" nl)
      (fmt #t "||" (space-to 3) "Package name" (space-to vercol) "SemVer" (space-to 36) "Synopsis"
           nl
           (pad-char #\= (space-to (max 1 (- terminal-cols 1))))
           nl)
      (vector-sort! (lambda (x y) (<? compare-names x y)) package-names)
      (vector-for-each
       (lambda (package-name)
         (let ((package (hashtable-ref packages package-name #f))
               (pkgname-len (string-length
                             (call-with-string-output-port
                               (lambda (p) (display package-name p))))))
           (for-each
            (lambda (version)
              (let ((version-locked?
                     (exists (lambda (project)
                               (project-locks-package-version? project version))
                             lockfile-projects))
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
                       (if (> pkgname-len (- vercol 4)) nl fmt-null)
                       (space-to (- vercol 1)) " "
                       (ellipses "…"
                                 (colorize (pad 12 (trim 11 (version-number version))))
                                 (trim (max 10 (- terminal-cols 37))
                                       (cond ((version-synopsis version) => car)
                                             (else "-"))))
                       nl))))
            (package-version* package))))
       package-names))))

(define (show-package manifest-filename lockfile-filename index-filename pkg-name)
  (let-values (((_ packages) (read-package-index index-filename '()))
               ((terminal-cols _terminal-lines) (get-terminal-size)))
    (let ((package (hashtable-ref packages pkg-name #f)))
      (unless package
        (error 'show-package "No package by that name" pkg-name))
      (let ((highest (last (package-version* package))))
        (fmt #t (fmt-underline (package-name package) " "
                               (version-number highest) " - "
                               (cond ((version-synopsis highest) => car)
                                     (else "(no synopsis)")))
             nl)
        (when (version-description highest)
          (fmt #t
               (with-width (- terminal-cols 2)
                           (fmt-join (lambda (paragraph)
                                       (if (string-prefix? " " paragraph)
                                           (cat nl paragraph nl)
                                           (cat nl (wrap-lines paragraph))))
                                     (version-description highest)))))

        (fmt #t nl (fmt-underline "Metadata" nl))
        (when (version-authors highest)
          (fmt #t (fmt-join (lambda (x)
                              (cat "Author:" (space-to 15) x nl))
                            (version-authors highest))))
        (when (version-homepage highest)
          (fmt #t "Homepage:" (space-to 15) (car (version-homepage highest)) nl))

        (letrec ((show-deps
                  (lambda (heading dep*)
                    (unless (null? dep*)
                      (fmt #t nl (fmt-underline heading) nl
                           (fmt-join (match-lambda
                                      [(dep-name dep-range)
                                       (cat dep-name " " (space-to 15)
                                            dep-range " "
                                            (space-to 40)
                                            "("
                                            (semver-range->string
                                             (semver-range-desugar
                                              (string->semver-range dep-range)))
                                            ")" nl)])
                                     dep*))))))
          (show-deps "Dependencies" (version-depends highest))
          (show-deps "Dependencies (development)" (version-depends/dev highest))
          (show-deps "Conflicts" (version-conflicts highest)))

        (let ((lock (version-lock highest)))
          (match (assq-ref lock 'location)
            [(('git remote-url))
             (fmt #t nl (fmt-underline "Source code" nl))
             (fmt #t "Git remote:" (space-to 15) remote-url nl
                  "Revision:" (space-to 15) (car (assq-ref lock 'revision)) nl)
             (cond ((assq-ref lock 'tag #f) =>
                    (lambda (tag)
                      (fmt #t "Tag:" (space-to 15) (car tag) nl))))]

            [(('url remote-url))
             (fmt #t nl (fmt-underline "Source code" nl))
             (fmt #t "URL:" (space-to 15) remote-url nl)
             (let ((content (assq-ref lock 'content)))
               (fmt #t "Content:" (space-to 15) (car content) nl))]))

        (fmt #t nl (fmt-underline "Available versions") nl
             (fmt-join (lambda (v) (cat (version-number v) nl))
                       (package-version* package))))))))
