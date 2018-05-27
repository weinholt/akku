#!r6rs #!r6rs
;;; compat.chezscheme.sls --- OS processes for Chez Scheme

;; Copyright (C) 2010, 2011, 2015 Andreas Rottmann <a.rottmann@gmx.at>
;; Copyright (C) 2018 Göran Weinholt <goran@weinholt.se>

;; Authors: Andreas Rottmann <a.rottmann@gmx.at>
;;          Göran Weinholt <goran@weinholt.se>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;; This does not work on Windows.

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
  (import (except (chezscheme) process)
          (spells pathname))

  (define dummy
    (case (machine-type)
      ((i3le ti3le a6le ta6le arm32le ppc32le) (load-shared-object "libc.so.6"))
      ((i3osx ti3osx a6osx ta6osx) (load-shared-object "libc.dylib"))
      ((i3nt ti3nt a3nt ta3nt) (load-shared-object "crtdll.dll"))
      (else (load-shared-object "libc.so"))))

  (define-record-type process
    (fields id input output errors))

  (define (x->string x)
    (cond ((string? x)   x)
          ((pathname? x) (->namestring x))
          (else
           (assertion-violation 'x->string "cannot coerce to string" x))))

  (define (x->bytevector x)
    (let ((bv (cond ((bytevector? x) x)
                    ((string? x) (string->utf8 x))
                    ((pathname? x) (string->utf8 (->namestring x)))
                    (else
                     (error 'x->c-string "cannot coerce to bytevector" x)))))
      (do ((i 0 (fx+ i 1)))
          ((fx=? i (bytevector-length bv)) bv)
        (when (zero? (bytevector-u8-ref bv i))
          (error 'bytevector->c-string
                 "not unrepresentable as a NUL-terminated string" x)))))

  ;; Converts a bytevector to a NUL-terminated foreign string (it must
  ;; later be foreign-free'd).
  (define (bv->c-string bv)
    (let* ((n (bytevector-length bv))
           (c-string (foreign-alloc (fx* (fx+ n 1) (foreign-sizeof 'unsigned-8)))))
      (do ((i 0 (fx+ i 1)))
          ((fx=? i n)
           (foreign-set! 'unsigned-8 c-string i 0)
           c-string)
        (foreign-set! 'unsigned-8 c-string i (bytevector-u8-ref bv i)))))

  (define (string-list->char** strings)
    (let* ((n (length strings))
           (c-strings (foreign-alloc (fx* (fx+ n 1) (foreign-sizeof 'void*)))))
      (do ((i 0 (fx+ i 1))
           (bytevectors (map x->bytevector strings) (cdr bytevectors)))
          ((fx=? i n)
           (foreign-set! 'void* c-strings (fx* i (foreign-sizeof 'void*)) 0)
           c-strings)
        (foreign-set! 'void* c-strings (fx* i (foreign-sizeof 'void*))
                      (bv->c-string (car bytevectors))))))

  (define (free-char** c-strings)
    (let lp ((i 0))
      (let ((c-string (foreign-ref 'void* c-strings (* i (foreign-sizeof 'void*)))))
        (unless (zero? c-string)
          (foreign-free c-string)
          (lp (+ i 1)))))
    (foreign-free c-strings))

  (define call/errno
    (let ((get-error
           (let ((%errno (foreign-entry "errno"))
                 (%strerror (foreign-procedure "strerror" (int) string)))
             (lambda () (%strerror (foreign-ref 'int %errno 0))))))
      (lambda (procedure . args)
        (critical-section
         (let* ((return-value (apply procedure args))
                (err (get-error)))
           (values return-value err))))))

  (define (execv prog arg*)
    (let ((%execv (foreign-procedure "execv" (string void*) int)))
      (let ((arg* (map x->bytevector (cons prog arg*))))
        (let ((%arg (string-list->char** arg*)))
          (let-values (((status err) (call/errno %execv prog %arg)))
            (free-char** %arg)
            err)))))

  (define (execve prog arg* env*)
    (let ((%execve (foreign-procedure "execve" (string void* void*) int)))
      (let ((arg* (map x->bytevector (cons prog arg*)))
            (env* (map x->bytevector env*)))
        (let ((%arg (string-list->char** arg*))
              (%env (string-list->char** env*)))
          (let-values (((status err) (call/errno %execve prog %arg %env)))
            (free-char** %arg)
            (free-char** %env)
            err)))))

  (define %fork (foreign-procedure "fork" () int))

  (define %close (foreign-procedure "close" (int) int))

  (define %dup (foreign-procedure "dup" (int) int))

  (define pipe
    (let ((%pipe (foreign-procedure "pipe" (void*) int)))
      (lambda ()
        (let ((&pipefd (foreign-alloc (* 2 (foreign-sizeof 'int)))))
          (let-values (((status err) (call/errno %pipe &pipefd)))
            (let ((pipefd-read (foreign-ref 'int &pipefd 0))
                  (pipefd-write (foreign-ref 'int &pipefd (foreign-sizeof 'int))))
              (foreign-free &pipefd)
              (unless (zero? status)
                (error 'pipe "Error from libc" err))
              (cons (open-fd-input-port pipefd-read)
                    (open-fd-output-port pipefd-write))))))))

  (define (spawn-process env stdin stdout stderr prog . args)
    (let* ((inports (and (not stdin) (pipe)))
           (outports (and (not stdout) (pipe)))
           (errports (and (not stderr) (pipe))))
      (let-values (((pid err) (call/errno %fork)))
        (cond ((= pid 0)                  ;child
               (let ((stdin (or stdin (begin
                                        (close-port (cdr inports))
                                        (car inports))))
                     (stdout (or stdout (begin
                                          (close-port (car outports))
                                          (cdr outports))))
                     (stderr (or stderr (begin
                                          (close-port (car errports))
                                          (cdr errports)))))
                 (unless (eqv? stdin (console-input-port))
                   (%close 0)
                   (%dup (port-file-descriptor stdin)))
                 (unless (eqv? stdout (console-output-port))
                   (%close 1)
                   (%dup (port-file-descriptor stdout)))
                 (unless (eqv? stderr (console-error-port))
                   (%close 2)
                   (%dup (port-file-descriptor stderr)))
                 (let* ((_SC_OPEN_MAX 4)  ;Linux-specific
                        (%sysconf (foreign-procedure "sysconf" (int) long))
                        (max-fds (%sysconf _SC_OPEN_MAX)))
                   (do ((i 3 (+ i 1)))
                       ((>= i max-fds))
                     (%close i))))
               (let ((errno (if env
                                (execve prog args (env-alist->strings env))
                                (execv prog args))))
                 (error 'spawn-process "Could not exec" prog errno)
                 (let ((%exit (foreign-procedure "exit" (int) void)))
                   (%exit 1))))
              ((= pid -1)
               (error 'spawn-process "Could not fork" err prog args))
              (else                       ;parent
               (unless stdin
                 (close-port (car inports)))
               (unless stdout
                 (close-port (cdr outports)))
               (unless stderr
                 (close-port (cdr errports)))
               (make-process pid
                             (and (not stdin) (cdr inports))
                             (and (not stdout) (car outports))
                             (and (not stderr) (car errports))))))))

  (define (env-alist->strings alist)
    (map (lambda (entry)
           (string-append (car entry) "=" (cdr entry)))
         alist))

  (define wait-for-process
    (let ((%waitpid (foreign-procedure "waitpid" (int void* int) int)))
      (lambda (process)
        (let ((&wstatus (foreign-alloc (foreign-sizeof 'int)))
              (options 0))
          (let* ((pid (%waitpid (process-id process) &wstatus options))
                 (wstatus (foreign-ref 'int &wstatus 0)))
            (foreign-free &wstatus)
            ;; XXX: This is specific to Linux.
            (let ((status (bitwise-bit-field wstatus 8 16))
                  (signal (bitwise-bit-field wstatus 0 7)))
              (if (zero? signal)
                  (values status #f)    ;normal exit
                  (values #f signal))))))))

  (define (run-shell-command cmd)
    (let ((status (system cmd)))
      (if (negative? status)
          (values #f (- status))        ;killed by a signal
          (values status #f)))))

;; Local Variables:
;; scheme-indent-styles: ((foreign-procedure 2))
;; End:
