#!/usr/bin/env scheme-script
;; -*- mode: scheme; coding: utf-8 -*- !#
;; Copyright © 2017-2019 Göran Weinholt <goran@weinholt.se>
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
  (srfi :37 args-fold)
  (wak fmt)
  (wak fmt color)
  (chibi match)
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
  (only (akku lib scripts) run-scripts)
  (only (akku lib update) update-index)
  (only (akku lib utils) path-join assq-ref assq-update assoc-replace
        application-data-directory system-data-directory
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

(define (check-args arg* cmd num)
  (case num
    ((0)
     (unless (null? arg*)
       (log/error "Too many command line arguments")
       (print-help cmd)
       (exit 1)))
    ((1)
     (unless (= (length arg*) 1)
       (log/error "Expected exactly one command line argument")
       (print-help cmd)
       (exit 1)))
    ((+)
     (unless (>= (length arg*) 1)
       (log/error "Too few command line arguments")
       (print-help cmd)
       (exit 1)))
    ((?)
     (unless (<= (length arg*) 1)
       (log/error "Too many command line arguments")
       (print-help cmd)
       (exit 1)))
    (else
     (error 'check-args "Internal error" arg* cmd num))))

(define print-help
  (case-lambda
    ((cmd)
     (print-help))
    (()
     (fmt (current-error-port)
          (fmt-yellow ",") (fmt-green "()") (fmt-white "´")
          " Akku.scm " main-package-version " - Scheme package manager" nl
          nl
          "USAGE: akku [OPTIONS] COMMAND" nl
          nl
          (fmt-underline "Options") nl
          "  --help        Print help" nl
          "  --debug       Enable debug level messages" nl
          "  --trace       Enable trace level messages" nl
          nl
          (fmt-underline "Simple commands") nl
          "  init <directory> - initialize a new project from a template" nl
          "  list - list all packages in the index" nl
          "  show <pkg> - show package information" nl
          "* install <pkg>+ - all-in-one add/lock/install a package" nl
          "* update - update the package index" nl
          nl
          (fmt-underline "Basic commands") nl
          "  add [OPTIONS] <pkg> - add a dependency to Akku.manifest" nl
          "  add [OPTIONS] <pkg>@<range> - add a versioned dependency" nl
          "    --dev              add a development dependency" nl
          "  lock - generate Akku.lock from Akku.manifest and the index" nl
          "* install - install dependencies according to Akku.lock" nl
          "  remove <pkg> - remove a dependency from Akku.manifest" nl
          "* uninstall <pkg>+ - all-in-one remove/lock/install" nl
          "  version - print Akku.scm's version number" nl
          nl
          (fmt-underline "Creative commands") nl
          "* publish [OPTIONS] - publish the current project" nl
          "    --version=VERSION  override the published version" nl
          "    --tag=TAG          override the published tag" nl
          "  scan [directory] - scan a repository and print what Akku sees" nl
          nl
          (fmt-underline "Advanced commands") nl
          "  graph [directory] - print a graphviz file showing library dependencies" nl
          "  dependency-scan [OPTIONS] <filename>+ - find dependencies" nl
          "    --implementation=xyz[,...]  implementation codenames " nl
          "  license-scan [OPTIONS] <filename>+ - scan for license notices " nl
          "    --implementation=xyz[,...]  implementation codenames " nl
          "  archive-scan <directory>+ - generate a package index" nl
          "  compat-scan <filename>+ - analyze implementation compatibility" nl
          nl
          " [*]: This command may make network requests." nl
          nl
          "Homepage: https://akkuscm.org/" nl
          "License: GNU GPLv3+" nl))))

(define (optname->string name)
  (cond ((char? name)
         (string #\- name))
        ((string? name)
         (string-append "--" name))
        (else name)))

(define (cmd-help arg* opts)
  (print-help)
  (exit 0))

(define (parse-package-name package-name)
  (if (char=? (string-ref package-name 0) #\()
      (read (open-string-input-port package-name))
      package-name))

(define (cmd-init arg* opts)
  (check-args arg* 'init 1)
  (init-project (car arg*))
  (fmt #t nl
       "To start developing in the new project, type this in your shell:" nl
       nl
       " cd " (car arg*) nl
       " akku install" nl
       " .akku/env" nl
       nl
       ;; TODO: This caveat should not be needed.
       (wrap-lines
        "Some implementations may require small adjustments to run
R6RS programs (e.g. guile -x .guile.sls -x .sls). See the manual for
your implementation.") nl))

(define (cmd-add arg* opts)
  (check-args arg* 'add '+)
  (let ((dev? (and (assq 'dev opts) #t)))
    (for-each
     (lambda (dep)
       (let-values (((package-name range)
                     (match (string-split dep #\@)
                       [(package-name range)
                        (values package-name range)]
                       [(package-name)
                        (values package-name #f)])))
         (let ((package-name (parse-package-name package-name)))
           (add-dependency manifest-filename (get-index-filename)
                           dev? package-name range))))
     arg*)))

(define (cmd-remove arg* opts)
  (check-args arg* 'remove '+)
  (remove-dependencies manifest-filename
                       (map parse-package-name arg*)))

(define (cmd-list arg* opts)
  (check-args arg* 'list 0)
  (list-packages manifest-filename lockfile-filename
                 (get-index-filename)))

(define (cmd-show arg* opts)
  (check-args arg* 'show 1)
  (show-package manifest-filename lockfile-filename
                (get-index-filename)
                (parse-package-name (car arg*))))

(define (cmd-lock arg* opts)
  (check-args arg* 'lock 0)
  (lock-dependencies manifest-filename
                     lockfile-filename
                     (get-index-filename)))

(define (cmd-install arg* opts)
  (cond
    ((null? arg*)
     ;; Install locked dependencies.
     (unless (file-exists? lockfile-filename)
       (cmd-lock '() opts))
     (fetch lockfile-filename)
     (let ((index-filename (get-index-filename)))
       (run-scripts lockfile-filename manifest-filename index-filename
                    '(pre-install))
       (install lockfile-filename manifest-filename)
       (run-scripts lockfile-filename manifest-filename index-filename
                    '(post-install))))
    (else
     ;; All-in-one automatic installation of a package.
     (cmd-add arg* opts)
     (cmd-lock '() opts)
     (cmd-install '() opts))))

(define (cmd-uninstall arg* opts)
  (check-args arg* 'uninstall '+)
  ;; All-in-one automatic removal of a package.
  (cmd-remove arg* '())
  (cmd-lock '() '())
  (cmd-install '() '()))

(define (cmd-version arg* opts)
  (check-args arg* 'version 0)
  (display main-package-version)
  (newline))

(define (cmd-publish arg* opts)
  (check-args arg* 'publish 0)
  (let ((version-override (assq-ref opts 'version))
        (tag-override (assq-ref opts 'tag)))
    (publish-packages manifest-filename "." '("https://akkuscm.org/")
                      version-override tag-override)))

(define (cmd-scan arg* opts)
  (check-args arg* 'scan '?)
  (scan-repository (if (null? arg*) "." (car arg*))))

(define (cmd-update arg* opts)
  (define repositories               ;TODO: should be in a config file
    '([(tag . akku)
       (url . "https://archive.akkuscm.org/archive/")
       (keyfile . "akku-archive-*.gpg")]))
  (check-args arg* 'update 0)
  (let ((index-fn (path-join (application-data-directory) "index.db"))
        (app-keys-dir (path-join (application-data-directory) "keys.d"))
        (sys-keys-dir (cond ((system-data-directory) =>
                             (lambda (dir) (path-join dir "keys.d")))
                            (else #f))))
    (update-index index-fn
                  (if sys-keys-dir
                      (list app-keys-dir sys-keys-dir)
                      (list app-keys-dir))
                  repositories)))

(define (cmd-graph arg* opts)
  (check-args arg* 'graph '?)
  (print-gv-file (if (null? arg*) "." (car arg*))))

(define (cmd-dependency-scan arg* opts)
  (check-args arg* 'dependency-scan '+)
  (let ((impl* (cond ((assq-ref opts 'implementation)
                      => (lambda (impl) (map string->symbol (string-split impl #\,))))
                     (else '()))))
    (when (null? impl*)
      (log/error "At least one implementation name must be provided")
      (print-help 'dependency-scan)
      (exit 1))
    (dependency-scan arg* impl*)))

(define (cmd-license-scan arg* opts)
  (check-args arg* 'license-scan '+)
  (let ((impl* (cond ((assq-ref opts 'implementation)
                      => (lambda (impl) (map string->symbol (string-split impl #\,))))
                     (else '()))))
    (when (null? impl*)
      (log/error "At least one implementation must be provided")
      (print-help 'license-scan)
      (exit 1))
    (license-scan arg* impl*)))

(define (cmd-archive-scan arg* opts)
  (check-args arg* 'scan '+)
  (archive-scan arg*))

(define (cmd-compat-scan arg* opts)
  (check-args arg* 'compat-scan '+)
  (compat-scan lockfile-filename arg*))

(define (set-log-level threshold)
  (set-logger-properties! logger:akku
                          `((threshold ,threshold)
                            (handlers
                             ,(lambda (entry)
                                (log-formatter entry (current-error-port)))))))

(define commands
  (list
   (cons 'add cmd-add)
   (cons 'archive-scan cmd-archive-scan)
   (cons 'compat-scan cmd-compat-scan)
   (cons 'dependency-scan cmd-dependency-scan)
   (cons 'graph cmd-graph)
   (cons 'help cmd-help)
   (cons 'init cmd-init)
   (cons 'install cmd-install)
   (cons 'license-scan cmd-license-scan)
   (cons 'list cmd-list)
   (cons 'lock cmd-lock)
   (cons 'publish cmd-publish)
   (cons 'remove cmd-remove)
   (cons 'scan cmd-scan)
   (cons 'show cmd-show)
   (cons 'uninstall cmd-uninstall)
   (cons 'update cmd-update)
   (cons 'version cmd-version)))

(define (opt names key required-arg? valid-for-commands)
  (option names required-arg? #f
          (lambda (opt name arg alist)
            (cond
              ((not (memq (cdr (assq 'cmd alist)) valid-for-commands))
               (log/error "Invalid option: " (optname->string name))
               (print-help (cdr (assq 'cmd alist)))
               (exit 1))
              ((and arg (not required-arg?))
               (log/error "Option does not take an argument: "
                          (optname->string name))
               (print-help (cdr (assq 'cmd alist)))
               (exit 1))
              ((and required-arg? (not arg))
               (log/error "Option requires an argument: "
                          (optname->string name))
               (print-help (cdr (assq 'cmd alist)))
               (exit 1))
              ((assq name alist)
               (log/error "Options can only be given once: "
                          (optname->string name))
               (print-help (cdr (assq 'cmd alist)))
               (exit 1))
              (else
               (cons (cons key arg) alist))))))

(define (parse-command-line cmdline)
  (args-fold (cdr cmdline)
             (list
              (opt '("dev") 'dev #f '(install))
              (opt '("version") 'version #t '(publish))
              (opt '("tag") 'tag #t '(publish))
              (opt '("implementation" #\i) 'implementation #t
                   '(dependency-scan license-scan))
              (option '("help" #\h) #f #f
                      (lambda (option name arg alist)
                        (print-help (cdr (assq 'cmd alist)))
                        (exit 0)))
              (option '("trace") #f #f
                      (lambda (option name arg alist)
                        (set-log-level 'trace)
                        alist))
              (option '("debug") #f #f
                      (lambda (option name arg alist)
                        (set-log-level 'debug)
                        alist)))
             (lambda (option name arg alist)
               (log/error "Unknown option: " (optname->string name))
               (print-help (cdr (assq 'cmd alist)))
               (exit 1))
             (lambda (arg alist)
               (if (cdr (assq 'cmd alist))
                   (assq-update alist 'arg*
                                (lambda (arg*) (cons arg arg*))
                                '())
                   (let ((cmd (string->symbol arg)))
                     (unless (assq cmd commands)
                       (log/error "Unrecognized command: " arg)
                       (cmd-help '() '()))
                     (assoc-replace alist 'cmd cmd))))
             '((arg* . ()) (cmd . #f))))

(define (main cmdline)
  (set-log-level (get-log-threshold))
  (let ((args (parse-command-line cmdline)))
    (let ((arg* (cdr (assq 'arg* args)))
          (cmd (cdr (assq 'cmd args)))
          (opts (filter (lambda (x) (not (memq (car x) '(arg* cmd))))
                        args)))
      (cond ((assq cmd commands) =>
             (lambda (cmd-def) ((cdr cmd-def) arg* opts)))
            (else
             (print-help)
             (exit 0))))))

(main (command-line))
