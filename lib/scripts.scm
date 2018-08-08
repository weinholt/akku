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

;; Script invocation.

(library (akku lib scripts)
  (export
    run-scripts
    logger:akku.scripts)
  (import
    (rnrs (6))
    (only (srfi :1 lists) append-map delete-duplicates)
    (wak fmt)
    (xitomatl AS-match)
    (akku private compat)
    (akku lib lock)
    (akku lib manifest)
    (akku lib schemedb)
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

;; Get all implementation names that show up in cond-expand feature
;; requirements.
(define (scripts-implementation-names scripts)
  (filter rnrs-implementation-name? (scripts-referenced-features scripts)))

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

(define (run-scripts lockfile-location manifest-filename feature-list)
  (define (run-cmd cmd)
    (cond
      ((cmd-nop? cmd)
       (log/debug "Script: NOP"))
      ((cmd-run? cmd)
       ;; Shell command. FIXME: should check the error status.
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
  (define (run package version raw-scripts feature-list)
    (let* ((scripts (map parse-script-expr raw-scripts))
           ;; XXX: Is this a good strategy?
           (impl* (scripts-implementation-names scripts))
           (feature-list* (if (null? impl*)
                              (list feature-list)
                              (map (lambda (impl)
                                     (cons impl feature-list))
                                   impl*))))
      ;; FIXME: Must change to the project's directory first.
      (for-each
       (lambda (feature-list^)
         (let ((to-run (scripts-expand scripts feature-list^)))
           (log/trace "Expanding scripts with feature list " feature-list)
           (for-each run-cmd to-run)))
       feature-list*)))

  ;; TODO: Run the scripts in the lockfile as well.

  ;; TODO: Ask the user if it's ok to run the script, if they did not
  ;; come from a trusted index.
  (let ((project* (read-lockfile lockfile-location))
        (manifest (if (file-exists? manifest-filename)
                      (read-manifest manifest-filename)
                      '()))
        (feature-list (cons 'linux feature-list)))
    ;; TODO: Add the OS to the feature list
    (log/trace "Running scripts from " (wrt lockfile-location)
               " and " (wrt manifest-filename)
               " with feature list " feature-list)
    (for-each (lambda (pkg)
                (for-each (lambda (ver)
                            (when (version-scripts ver)
                              (run pkg ver (version-scripts ver) feature-list)))
                          (package-version* pkg)))
              manifest))))
