#!r6rs #!r6rs
;;; compat.guile.sls --- process compat library for Guile

;; Copyright (C) 2010, 2011, 2015 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

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

          get-process-id
          
          run-shell-command)
  (import (rnrs base)
          (rnrs control)
          (rnrs io ports)
          (rnrs lists)
          (rnrs arithmetic bitwise)
          (srfi :8 receive)
          (srfi :9 records)
          (spells pathname)
          (prefix (only (guile)
                        execle
                        waitpid
                        system
                        getpid
                        primitive-fork
                        port-for-each
                        pipe
                        close
                        dup
                        file-port?
                        fileno
                        fcntl
                        F_GETFD
                        F_SETFD
                        FD_CLOEXEC
                        status:exit-val
                        status:term-sig
                        execl
                        execle)
                  guile:))

  (define-record-type process
    (make-process pid input output errors)
    process?
    (pid process-id)
    (input process-input)
    (output process-output)
    (errors process-errors))

  (define (->strlist who lst)
    (map (lambda (s)
           (cond ((string? s)   s)
                 ((pathname? s) (->namestring s))
                 (else
                  (assertion-violation who "cannot coerce to string list" lst))))
         lst))

  (define (spawn-process env stdin stdout stderr prog . args)
    (let* ((argv (->strlist 'spawn-process (cons prog args)))
           (inports (and (not stdin) (guile:pipe)))
           (outports (and (not stdout) (guile:pipe)))
           (errports (and (not stderr) (guile:pipe)))
           (pid (guile:primitive-fork)))
      (cond ((= 0 pid)
             ;; Child
             (let ((stdin (or stdin (begin
                                      (close-port (cdr inports))
                                      (car inports))))
                   (stdout (or stdout (begin
                                        (close-port (car outports))
                                        (cdr outports))))
                   (stderr (or stderr (begin
                                        (close-port (car errports))
                                        (cdr errports)))))
               (unless (= 0 (guile:fileno stdin))
                 (guile:close 0)
                 (guile:dup (guile:fileno stdin)))
               (unless (= 1 (guile:fileno stdout))
                 (guile:close 1)
                 (guile:dup (guile:fileno stdout)))
               (unless (= 2 (guile:fileno stderr))
                 (guile:close 2)
                 (guile:dup (guile:fileno stderr)))
               (guile:port-for-each
                (lambda (port)
                  (when (and (guile:file-port? port)
                             (not (memv (guile:fileno port) '(0 1 2))))
                    (guile:fcntl port
                                 guile:F_SETFD
                                 (bitwise-ior
                                  guile:FD_CLOEXEC
                                  (guile:fcntl port guile:F_GETFD)))))))
             (if env
                 (apply guile:execle (car argv) (env-alist->strings env) argv)
                 (apply guile:execl (car argv) argv)))
            (else
             (unless stdin
               (close-port (car inports)))
             (unless stdout
               (close-port (cdr outports)))
             (unless stderr
               (close-port (cdr errports)))
             (make-process pid
                           (and (not stdin) (cdr inports))
                           (and (not stdout) (car outports))
                           (and (not stderr) (car errports)))))))

  (define (env-alist->strings alist)
    (map (lambda (entry)
           (string-append (car entry) "=" (cdr entry)))
         alist))
  
  (define (status->values status)
    (values (guile:status:exit-val status)
            (guile:status:term-sig status)))
  
  (define (wait-for-process process)
    (status->values (cdr (guile:waitpid (process-id process)))))

  (define (get-process-id)
    (guile:getpid))

  (define (run-shell-command cmd)
    (status->values (guile:system cmd)))

  )
