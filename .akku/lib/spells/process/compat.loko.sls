#!r6rs
;;; compat.loko.sls --- OS processes for Loko Scheme

;; Copyright (C) 2020 Göran Weinholt <goran@weinholt.se>

;; Authors: Göran Weinholt <goran@weinholt.se>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(library (spells process compat)
  (export process?
          process-id
          process-input
          process-output
          process-errors

          spawn-process
          wait-for-process

          (rename (srfi:get-process-id get-process-id))

          run-shell-command)
  (import (rnrs)
          (spells pathname)
          (prefix (pre-srfi processes) srfi:))

(define (pipe)
  (let-values (((in out) (srfi:make-pipe)))
    (cons in out)))

(define-record-type process
  (fields id input output errors proc))

(define (spawn-process* setup env stdin stdout stderr  prog . args)
  (let* ((inports (and (not stdin) (pipe)))
         (outports (and (not stdout) (pipe)))
         (errports (and (not stderr) (pipe)))
         (setup (append (list 'stdin (or stdin (car inports))
                              'stdout (or stdout (cdr outports))
                              'stderr (or stderr (cdr errports))
                              'env env)
                        setup))
         (proc (apply srfi:make-process setup prog args)))
    (unless stdin
      (close-port (car inports)))
    (unless stdout
      (close-port (cdr outports)))
    (unless stderr
      (close-port (cdr errports)))
    (make-process (srfi:process-child-id proc)
                  (and (not stdin) (cdr inports))
                  (and (not stdout) (car outports))
                  (and (not stderr) (car errports))
                  proc)))

(define (spawn-process env stdin stdout stderr prog . args)
  (apply spawn-process* '() env stdin stdout stderr prog args))

(define (wait-for-process process)
  (let ((proc (process-proc process)))
    (srfi:process-wait proc)
    (cond
      ((srfi:process-exit-code proc)
       => (lambda (status) (values status #f)))
      ((srfi:process-terminate-signal proc)
       => (lambda (signal) (values #f signal))))))

(define (run-shell-command cmd)
  (wait-for-process (spawn-process* (list 'arg0 "sh")
                                    #f (current-input-port)
                                    (current-output-port)
                                    (current-error-port)
                                    "/bin/sh" "-c" cmd))))
