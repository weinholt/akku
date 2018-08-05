#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
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

;; Main command line interface to Akku.scm.

(import
  (rnrs (6))
  (industria strings)
  (only (spells logging) set-logger-properties! log-entry-level-name
        log-entry-object default-log-formatter)
  (only (srfi :13 strings) string-prefix?)
  (wak fmt)
  (wak fmt color)
  (xitomatl AS-match)
  (akku lib bundle)
  (only (akku format lockfile) lockfile-filename)
  (only (akku format manifest) manifest-filename)
  (only (akku lib archive-maint) archive-scan)
  (only (akku lib fetch) fetch)
  (only (akku lib graph) print-gv-file)
  (only (akku lib init) init-project)
  (only (akku lib install) install logger:akku.install)
  (only (akku lib lock) logger:akku.lock lock-dependencies
        add-dependency remove-dependencies list-packages
        show-package)
  (only (akku lib publish) publish-packages)
  (only (akku lib scan) scan-repository)
  (only (akku lib update) update-index)
  (only (akku lib utils) path-join application-home-directory
        get-log-threshold get-index-filename)
  (only (akku metadata) main-package-version)
  (akku private logging))

(define logger:akku.main (make-logger logger:akku 'main))
(define log/info (make-fmt-log logger:akku.lock 'info))
(define log/warn (make-fmt-log logger:akku.lock 'warning))
(define log/error (make-fmt-log logger:akku.lock 'error))
(define log/debug (make-fmt-log logger:akku.lock 'debug))

(define (log-formatter entry port)
  (put-char port #\[)
  (let ((level (log-entry-level-name entry)))
    (fmt port (case level
                ((debug) (fmt-bold level))
                ((info) (fmt-green level))
                ((warning) (fmt-yellow level))
                ((error) (fmt-red level))
                ((critical) (fmt-bold (fmt-red level)))
                (else level))))
  (put-string port "] ")
  (let ((obj (log-entry-object entry)))
    (if (procedure? obj)
        (obj port)
        (display obj port)))
  (newline port)
  (flush-output-port port))

(define (cmd-help)
  (fmt (current-error-port)
       (fmt-yellow ",") (fmt-green "()") (fmt-white "´")
       " Akku.scm " main-package-version " - Scheme package manager" nl
       nl
       (fmt-underline "Simple usage") nl
       "   akku init <directory> - initialize a new project from a template" nl
       "   akku list - list all packages in the index" nl
       "   akku show <pkg> - show package information" nl
       " * akku install <pkg>+ - all-in-one add/lock/install a package" nl
       " * akku update - update the package index" nl
       nl
       (fmt-underline "Basic usage") nl
       "   akku add <pkg> - add a dependency to Akku.manifest" nl
       "   akku add <pkg>@<range> - add a versioned dependency" nl
       "   akku add --dev <pkg> - add a development dependency" nl
       "   akku lock - generate Akku.lock from Akku.manifest and the index" nl
       " * akku install - install dependencies according to Akku.lock" nl
       "   akku remove <pkg> - remove a dependency from Akku.manifest" nl
       " * akku uninstall <pkg>+ - all-in-one remove/lock/install" nl
       "   akku version - print Akku.scm's version number" nl
       nl
       (fmt-underline "Creative usage") nl
       " * akku publish [--version=x.y.z] [--tag=v] - publish the current project" nl
       "   akku scan [directory] - scan a repository and print what Akku sees" nl
       nl
       (fmt-underline "Advanced usage") nl
       "   akku graph - print a graphviz file showing library dependencies" nl
       "   akku dependency-scan <filename>+ - print source code dependencies" nl
       "   akku license-scan <filename>+ - scan dependencies for notices" nl
       "   akku archive-scan <directory>+ - generate a package index" nl
       nl
       " [*]: This command may make network requests." nl
       nl
       "Homepage: https://akkuscm.org/" nl
       "License: GNU GPLv3" nl nl)
  (exit 0))

(define (parse-package-name package-name)
  (if (char=? (string-ref package-name 0) #\()
      (read (open-string-input-port package-name))
      package-name))

(define (cmd-init arg*)
  (when (or (null? arg*) (not (null? (cdr arg*))))
    (cmd-help))
  (init-project (car arg*))
  (fmt #t nl
       "To start developing in the new project, type this in your shell:" nl
       nl
       " cd " (car arg*) nl
       " akku install" nl
       " source .akku/bin/activate" nl
       nl
       ;; TODO: This caveat should not be needed.
       (wrap-lines
        "The programs will work out of the box if Chez Scheme is
installed. Other implementations may require small adjustments to run
R6RS programs (e.g. guile -x .guile.sls -x .sls). See the manual for
your implementation.") nl))

(define (cmd-add arg*)
  (when (null? arg*)
    (cmd-help))
  (let ((dev? (member "--dev" arg*))
        (dep* (remove "--dev" arg*)))
    (for-each
     (lambda (dep)
       (let-values (((package-name range)
                     (match (string-split dep #\@)
                       [(package-name range) (values package-name range)]
                       [(package-name) (values package-name #f)])))
         (let ((package-name (parse-package-name package-name)))
           (add-dependency manifest-filename (get-index-filename)
                           dev? package-name range))))
     dep*)))

(define (cmd-remove arg*)
  (when (null? arg*)
    (cmd-help))
  (remove-dependencies manifest-filename (map parse-package-name arg*)))

(define (cmd-list arg*)
  (unless (null? arg*)
    (cmd-help))
  (list-packages manifest-filename lockfile-filename (get-index-filename)))

(define (cmd-show arg*)
  (unless (= (length arg*) 1)
    (cmd-help))
  (show-package manifest-filename lockfile-filename (get-index-filename) (car arg*)))

(define (cmd-lock arg*)
  (unless (null? arg*)
    (cmd-help))
  (lock-dependencies manifest-filename
                     lockfile-filename
                     (get-index-filename)))

(define (cmd-install arg*)
  (cond ((null? arg*)
         ;; Install locked dependencies.
         (unless (file-exists? lockfile-filename)
           (cmd-lock '()))
         (fetch lockfile-filename)
         (install lockfile-filename manifest-filename))
        (else
         ;; All-in-one automatic installation of a package.
         (cmd-add arg*)
         (cmd-lock '())
         (cmd-install '()))))

(define (cmd-uninstall arg*)
  (when (null? arg*)
    (cmd-help))
  ;; All-in-one automatic removal of a package.
  (cmd-remove arg*)
  (cmd-lock '())
  (cmd-install '()))

(define (cmd-version arg*)
  (unless (null? arg*)
    (log/error "Unrecognized command line arguments")
    (cmd-help))
  (display main-package-version)
  (newline))

(define (get-option arg* long-opt-prefix) ;TODO: again, buy a better cmdline parser
  (cond ((memp (lambda (x) (string-prefix? long-opt-prefix x)) arg*)
         => (lambda (arg*) (substring (car arg*) (string-length long-opt-prefix)
                                      (string-length (car arg*)))))
        (else #f)))

(define (cmd-publish arg*)
  (let ((version-override (get-option arg* "--version="))
        (tag-override (get-option arg* "--tag=")))
    (publish-packages manifest-filename "." '("https://akkuscm.org/")
                      version-override tag-override)))

(define (cmd-scan arg*)
  (when (> (length arg*) 1)
    (cmd-help))
  (scan-repository (if (null? arg*) "." (car arg*))))

(define (cmd-update arg*)
  (define repositories                  ;TODO: should be in a config file
    '([(tag . akku)
       (url . "https://archive.akkuscm.org/archive/")
       (keyfile . "akku-archive-*.gpg")]))
  (define keys-directory (path-join (application-home-directory) "share/keys.d"))
  (define index-filename (path-join (application-home-directory) "share/index.db"))
  (update-index index-filename keys-directory repositories))

(define (cmd-graph . _)
  (print-gv-file "."))

(define (cmd-dependency-scan arg*)
  (let ((impl* (cond ((get-option arg* "--implementation=")
                      => (lambda (impl) (map string->symbol (string-split impl #\,))))
                     (else '())))
        (arg* (filter (lambda (x) (not (string-prefix? "-" x))) arg*)))
    (when (null? arg*)
      (log/error "At least one program or library entry point must be provided")
      (cmd-help))
    (dependency-scan arg* impl*)))

(define (cmd-license-scan arg*)
  (let ((impl* (cond ((get-option arg* "--implementation=")
                      => (lambda (impl) (map string->symbol (string-split impl #\,))))
                     (else '())))
        (arg* (filter (lambda (x) (not (string-prefix? "-" x))) arg*)))
    (when (null? arg*)
      (log/error "At least one program or library entry point must be provided")
      (cmd-help))
    (license-scan arg* impl*)))

(define (cmd-archive-scan arg*)
  (when (null? arg*)
    (log/error "At least one directory name must be provided")
    (cmd-help))
  (archive-scan arg*))

(set-logger-properties! logger:akku
                        `((threshold ,(get-log-threshold))
                          (handlers
                           ,(lambda (entry)
                              (log-formatter entry (current-error-port))))))

;; TODO: get a real command line parser.
(cond
  ((null? (cdr (command-line)))
   (cmd-help))
  ((string=? (cadr (command-line)) "init")
   (cmd-init (cddr (command-line))))
  ((string=? (cadr (command-line)) "add")
   (cmd-add (cddr (command-line))))
  ((string=? (cadr (command-line)) "remove")
   (cmd-remove (cddr (command-line))))
  ((string=? (cadr (command-line)) "list")
   (cmd-list (cddr (command-line))))
  ((string=? (cadr (command-line)) "show")
   (cmd-show (cddr (command-line))))
  ((string=? (cadr (command-line)) "lock")
   (cmd-lock (cddr (command-line))))
  ((string=? (cadr (command-line)) "install")
   (cmd-install (cddr (command-line))))
  ((string=? (cadr (command-line)) "uninstall")
   (cmd-uninstall (cddr (command-line))))
  ((string=? (cadr (command-line)) "version")
   (cmd-version (cddr (command-line))))
  ((string=? (cadr (command-line)) "publish")
   (cmd-publish (cddr (command-line))))
  ((string=? (cadr (command-line)) "scan")
   (cmd-scan (cddr (command-line))))
  ((string=? (cadr (command-line)) "update")
   (cmd-update (cddr (command-line))))
  ((string=? (cadr (command-line)) "graph")
   (cmd-graph (cddr (command-line))))
  ((string=? (cadr (command-line)) "dependency-scan")
   (cmd-dependency-scan (cddr (command-line))))
  ((string=? (cadr (command-line)) "license-scan")
   (cmd-license-scan (cddr (command-line))))
  ((string=? (cadr (command-line)) "archive-scan")
   (cmd-archive-scan (cddr (command-line))))
  (else
   (log/error "Unrecognized command: " (cadr (command-line)))
   (cmd-help)))
