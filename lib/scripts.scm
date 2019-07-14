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

;; Script invocation.

#|

Security goals for scripts

* If a script appears in the lockfile but that script does not match
  what is in the index, then the user is alerted and asked for ok.

* If the current project has scripts then the user is asked for ok.
  This should be possible to override with a command line argument.

|#

(library (akku lib scripts)
  (export
    run-scripts
    logger:akku.scripts)
  (import
    (rnrs (6))
    (only (srfi :1 lists) append-map delete-duplicates)
    (wak fmt)
    (chibi match)
    (only (akku lib fetch) project-source-directory)
    (only (akku lib install) akku-directory ffi-libraries-directory
          libraries-directory r7rs-libraries-directory
          binaries-directory)
    (akku lib lock)
    (akku lib manifest)
    (akku lib schemedb)
    (akku lib utils)
    (akku private compat)
    (akku private logging))

(define logger:akku.scripts (make-logger logger:akku 'scripts))
(define log/info (make-fmt-log logger:akku.scripts 'info))
(define log/warn (make-fmt-log logger:akku.scripts 'warning))
(define log/debug (make-fmt-log logger:akku.scripts 'debug))
(define log/trace (make-fmt-log logger:akku.scripts 'trace))

;; Expressions in the scripts declaration.
(define-record-type cmd-nop (sealed #t))
(define-record-type cmd-run (sealed #t) (fields cmd))
(define-record-type condexp (sealed #t) (fields clause*))
(define-record-type condexp-clause (sealed #t) (fields req body))

(define (parse-script-expr script)
  (match script
    [('cond-expand . clause*)
     (make-condexp
      (let lp ((clause* clause*))
        (match clause*
          [(('else body ...))
           (list (make-condexp-clause '(and) (map parse-script-expr body)))]
          [((req body ...) . clause*)
           (cons (make-condexp-clause req (map parse-script-expr body)) (lp clause*))]
          [() '()]
          [x
           (assertion-violation 'parse-script-expr
                                "Syntax error in script cond-expand" script x)])))]
    ;; [('workdir (? string? dir)) (make-cmd-workdir dir)]
    ;; [('env (? string? name) value) (make-cmd-env name value)]
    [('run (? string? cmd)) (make-cmd-run cmd)]

    [else
     (assertion-violation 'parse-script-expr
                          "Syntax error in script declaration" script)]))

;; Get all referenced feature names from cond-expand clauses.
(define (scripts-referenced-features scripts)
  (define flatten-feature-req
    (match-lambda
     [(? symbol? feature) (list feature)]
     [('and req* ...) (append-map flatten-feature-req req*)]
     [('or req* ...) (append-map flatten-feature-req req*)]
     [('not req) (flatten-feature-req req)]
     [x
      (assertion-violation 'scripts-referenced-features
                           "Syntax error in script declaration" scripts x)]))
  (define (extract decl)
    (cond
      ((condexp? decl)
       (append-map
        (lambda (clause)
          (append (flatten-feature-req (condexp-clause-req clause))
                  (extract (condexp-clause-body clause))))
        (condexp-clause* decl)))
      ((pair? decl)
       (append-map extract decl))
      (else '())))
  (delete-duplicates (extract scripts) eq?))

(define (scripts-expand scripts feature-list)
  (define ev
    (match-lambda
     [(? symbol? feature) (memq feature feature-list)]
     [('and req* ...) (for-all ev req*)]
     [('or req* ...) (exists ev req*)]
     [('not req) (not (ev req))]
     [x
      (assertion-violation 'scripts-expand
                           "Syntax error in script declaration" x)]))
  (define (condexp-clause-req-eval clause)
    (ev (condexp-clause-req clause)))
  (define (expand script)
    (cond
      ((condexp? script)
       (cond ((find condexp-clause-req-eval (condexp-clause* script))
              => (lambda (clause)
                   (scripts-expand (condexp-clause-body clause) feature-list)))
             (else (list (make-cmd-nop)))))
      ((pair? script)
       (append-map expand script))
      (else (list script))))
  (append-map expand scripts))

(define (prompt-user-y/n? prompt)
  (let lp ()
    (display prompt)
    (display " (y/N)? ")
    (flush-output-port (current-output-port))
    (let ((line (get-line (current-input-port))))
      (cond ((eof-object? line) #f)
            ((string=? line "") #f)
            ((memv (char-downcase (string-ref line 0)) '(#\y #\j #\д)) #t)
            ((memv (char-downcase (string-ref line 0)) '(#\n #\н)) #f)
            (else (lp))))))

(define (projects->scripts project*)
  (append-map
   (lambda (project)
     (if (null? (project-scripts project))
         '()
         (list (cons project (project-scripts project)))))
   project*))

(define (packages->scripts package*)
  (append-map
   (lambda (pkg)
     (append-map
      (lambda (version)
        (if (null? (version-scripts version))
            '()
            (list (cons #f (version-scripts version)))))
      (package-version* pkg)))
   package*))

(define (package-index-find-project index-filename project)
  ;; TODO: There are too many instances of procedures like this one.
  (call-with-input-file index-filename
    (lambda (p)
      (let lp ()
        (match (read p)
          ((? eof-object?) #f)
          (('package ('name name)
                     ('versions version* ...))
           (let ((matches (filter (lambda (version)
                                    (project-locks-package-version? project version))
                                  (map parse-version version*))))
             (if (null? matches)
                 (lp)
                 matches)))
          (else (lp)))))))  ;allow for future expansion

(define (run-scripts lockfile-location manifest-filename index-filename feature-list)
  (define (run-cmd cmd env)
    (cond
      ((cmd-nop? cmd))
      ((cmd-run? cmd)
       ;; Shell command. FIXME: should check the error status and akku
       ;; should not leak environment variables.
       (for-each
        (match-lambda
         [(name . value)
          (putenv name value)])
        env)
       (let-values (((stdin stdout stderr pid)
                     (open-process-ports (string-append (cmd-run-cmd cmd) " 2>&1")
                                         (buffer-mode line)
                                         (native-transcoder))))
         (log/info "Running " (wrt (cmd-run-cmd cmd)))
         (log/debug "Process started with pid " pid)
         (close-port stdin)
         (close-port stderr)
         (let lp ()
           (unless (port-eof? stdout)
             (log/debug "Script output: " (wrt (get-line stdout)))
             (lp)))))
      (else
       (error 'run "Invalid expression" cmd))))
  (define project&cmd-not-vetted?
    (match-lambda
     [(#f . cmd)
      (log/warn "The scripts of the current project HAVE NOT been vetted by the Akku.scm repository maintainer")
      (cmd-run? cmd)]      ;run cmd in the current project, not vetted
     [(project . cmd)
      ;; Compare with the index. Is the result different from what
      ;; "akku lock" would have created?
      (let ((version* (package-index-find-project index-filename project)))
        (cond
          ((null? version*)
           (log/warn "The project " (project-name project) " in " lockfile-location " is not in the index (try: akku update)")
           (log/warn "The scripts below MAY NOT have been vetted by the Akku.scm repository maintainer")
           #t)
          ((not (exists (lambda (version) (equal? (version-scripts version) (project-scripts project)))
                        version*))
           (log/warn "The project " (project-name project) " from " lockfile-location " has scripts that DIFFER from the index (try: akku lock)")
           (log/warn "There may be a good reason for this, but dishonesty may also be involved")
           #t)
          (else #f)))]))
  (define (get-approval-to-run project&cmd*)
    ;; Show any commands that have not been vetted and get permission
    ;; to run them. TODO: Get permission with -y on the command line.
    (let ((not-vetted (filter project&cmd-not-vetted? project&cmd*)))
      (cond
        ((null? not-vetted)
         'all-commands-vetted)
        (else
         (for-each
          (match-lambda
           [(project . cmd)
            (cond ((cmd-run? cmd)
                   (fmt #t "Shell command in "
                        (if project (project-name project) "the current project")
                        ": " nl
                        " " (wrt (cmd-run-cmd cmd)) nl)))])
          not-vetted)
         (cond
           ((not (prompt-user-y/n? "Run these commands"))
            (log/warn "User said no to running scripts, continuing without them")
            #f)
           (else 'user-ok))))))
  (let ((project* (read-lockfile lockfile-location))
        (manifest (if (file-exists? manifest-filename)
                      (read-manifest manifest-filename)
                      '()))
        (feature-list (cons (os-name) feature-list))
        (cwd (getcwd)))
    (let* ((akku-path-root (path-join cwd (akku-directory)))
           (akku-path-bin (path-join cwd (binaries-directory)))
           (akku-path-lib-ffi (path-join cwd (ffi-libraries-directory)))
           (akku-path-lib-r6rs (path-join cwd (libraries-directory)))
           (akku-path-lib-r7rs (path-join cwd (r7rs-libraries-directory)))
           (env (list (cons "akku_path_root" akku-path-root)
                      (cons "akku_path_bin" akku-path-bin)
                      (cons "akku_path_lib_ffi" akku-path-lib-ffi)
                      (cons "akku_path_lib_r6rs" akku-path-lib-r6rs)
                      (cons "akku_path_lib_r7rs" akku-path-lib-r7rs))))
      (log/trace "Running scripts from " (wrt lockfile-location)
                 " and " (wrt manifest-filename)
                 " with feature list " feature-list
                 " and environment " (wrt env))
      (let* ((project&scripts (append (projects->scripts project*)
                                      (packages->scripts manifest)))
             (project&cmd* (append-map
                            (match-lambda
                             [(project . scripts)
                              (map (lambda (script)
                                     (cons project script))
                                   (scripts-expand (map parse-script-expr scripts)
                                                   feature-list))])
                            project&scripts)))
        (log/trace "Projects and commands: " (wrt project&cmd*))
        (unless (null? project&cmd*)
          (when (get-approval-to-run project&cmd*)
            ;; Run them.
            (for-each mkdir/recursive (list akku-path-root akku-path-bin akku-path-lib-ffi
                                            akku-path-lib-r6rs akku-path-lib-r7rs))
            (for-each
             (match-lambda
              [(project . cmd)
               (log/trace "Running " cmd " in " project)
               (with-working-directory (if project
                                           (project-source-directory project)
                                           ".")
                                       (lambda ()
                                         (run-cmd cmd env)))])
             project&cmd*))))))))
