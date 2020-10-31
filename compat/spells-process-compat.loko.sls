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
                              'stderr (or stderr (cdr errports)))
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



#;

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
  (import (rnrs)
          (spells pathname)
          (only (loko system unsafe) bytevector-address)
          (only (loko) port-file-descriptor collections)
          (loko match)
          (loko system fibers)
          (only (loko system $host) enable-signal acknowledge-signal
                wait-signal-operation)
          (loko arch amd64 linux-syscalls)
          (loko arch amd64 linux-numbers)
          (srfi :98)
          (srfi :170)
          (only (loko system fibers) get-message))

  (define-record-type process
    (fields id input output errors waitch))

  (define (x->utf8z val)
    (let ((x val))
      (cond ((bytevector? x)
             (call-with-bytevector-output-port
               (lambda (p)
                 (do ((i 0 (fx+ i 1)))
                     ((fx=? i (bytevector-length x)))
                   (when (eqv? 0 (bytevector-u8-ref x i))
                     (error 'x->utf8z "unrepresentable as a NUL-terminated string" val)))
                 (put-bytevector p x)
                 (put-u8 p (char->integer #\nul)))))
            ((string? x) (x->utf8z (string->utf8 x)))
            ((pathname? x) (x->utf8z (string->utf8 (->namestring x))))
            (else (error 'x->utf8z "cannot coerce to bytevector" x)))))

  (define (bytevector-addresses bv*)
    (sint-list->bytevector (append (map bytevector-address bv*) '(0))
                           (native-endianness)
                           sizeof-void*))

  (define (env-alist->bytevectors alist)
    (map (lambda (entry)
           (call-with-bytevector-output-port
             (lambda (p)
               (if (bytevector? (car entry))
                   (put-bytevector p (car entry))
                   (put-bytevector p (string->utf8 (car entry))))
               (put-u8 p (char->integer #\=))
               (put-bytevector p (x->utf8z (cdr entry))))))
         alist))

  (define (execve prog arg* env*)
    (let retry ()
      (let ((gc0 (collections)))
        (let* ((fd (and (port? prog) (port-file-descriptor prog)))
               (prog (if fd #vu8(0) prog))
               (%arg (bytevector-addresses arg*))
               (%env (bytevector-addresses env*)))
          (if (not (= gc0 (collections)))
              (retry)                 ;bytevectors may have moved
              (sys_execveat (or fd AT_FDCWD) (bytevector-address prog)
                            (bytevector-address %arg)
                            (bytevector-address %env)
                            (if fd AT_EMPTY_PATH 0)
                            (lambda (errno)
                              (if (eqv? errno EINTR) (retry) errno))))))))

  (define (pipe)
    (let ((buf (make-bytevector (* 2 sizeof-int))))
      (sys_pipe2 (bytevector-address buf) (fxior O_CLOEXEC O_NONBLOCK))
      (cons (fdes->binary-input-port (bytevector-u32-native-ref buf 0))
            (fdes->binary-output-port (bytevector-u32-native-ref buf 4)))))

  (define (spawn-process env stdin stdout stderr prog . args)
    (let ((prog^ (x->utf8z prog))
          (args^ (map x->utf8z (cons prog args)))
          (env^ (env-alist->bytevectors (or env (get-environment-variables)))))
      (let* ((inports (and (not stdin) (pipe)))
             (outports (and (not stdout) (pipe)))
             (errports (and (not stderr) (pipe)))
             (errnoports (pipe)))
        (let ((pid (sys_fork)))
          (cond ((eqv? pid 0)             ;child
                 (guard (exn (else #f))
                   (let ((stdin (or stdin (begin
                                            (close-port (cdr inports))
                                            (car inports))))
                         (stdout (or stdout (begin
                                              (close-port (car outports))
                                              (cdr outports))))
                         (stderr (or stderr (begin
                                              (close-port (car errports))
                                              (cdr errports)))))
                     (let ((stdin^ (port-file-descriptor stdin))
                           (stdout^ (port-file-descriptor stdout))
                           (stderr^ (port-file-descriptor stderr)))
                       (when (and (fixnum? stdin^) (not (eqv? STDIN_FILENO stdin^)))
                         (sys_dup3 stdin^ STDIN_FILENO 0))
                       (when (and (fixnum? stdout^) (not (eqv? STDOUT_FILENO stdout^)))
                         (sys_dup3 stdout^ STDOUT_FILENO 0))
                       (when (and (fixnum? stderr^) (not (eqv? STDERR_FILENO stderr^)))
                         (sys_dup3 stderr^ STDERR_FILENO 0))))
                   (let ((errno (execve prog^ args^ env^)))
                     (put-bytevector (cdr errnoports)
                                     (uint-list->bytevector (list errno)
                                                            (native-endianness) 2))
                     (close-port (cdr errnoports))
                     (sys_exit EX_OSERR)))
                 (sys_exit EX_SOFTWARE))
                (else                     ;parent
                 (unless stdin
                   (close-port (car inports)))
                 (unless stdout
                   (close-port (cdr outports)))
                 (unless stderr
                   (close-port (cdr errports)))
                 (close-port (cdr errnoports))
                 (let* ((waitch (sigchld-subscribe pid))
                        (err (get-bytevector-n (car errnoports) 2)))
                   (close-port (car errnoports))
                   (if (and (bytevector? err) (eqv? (bytevector-length err) 2))
                       (raise           ;smuggled errno from child
                         (condition
                          (make-who-condition 'spawn-process)
                          (make-message-condition "Failed to exec program")
                          (make-irritants-condition (list env stdin stdout stderr prog args))
                          (make-syscall-error 'execveat (bytevector-u16-native-ref err 0))))
                       (make-process pid
                                     (and (not stdin) (cdr inports))
                                     (and (not stdout) (car outports))
                                     (and (not stderr) (car errports))
                                     waitch)))))))))

  (define (wait-for-process process)
    (let lp ()
      (let ((msg (get-message (process-waitch process))))
        (let ((code (cdr (assq 'code msg)))
              (status (cdr (assq 'status msg))))
          (cond
            ((eqv? code CLD_EXITED)
             (values status #f))
            ((eqv? code CLD_KILLED) (eqv? code CLD_DUMPED)
             ;; XXX: Is this also the case for CLD_DUMPED?
             (values #f status))
            (else
             ;; The child is still alive, but it stopped or continued
             (lp)))))))

  (define (get-process-id)
    (sys_getpid))

  (define (run-shell-command cmd)
    (wait-for-process (spawn-process #f (current-input-port)
                                     (current-output-port)
                                     (current-error-port)
                                     "/bin/sh" "-c" cmd)))

  (define chld-sub-ch (make-channel))

  (define (sigchld-subscribe pid)
    (let ((ch (make-channel)))
      (put-message chld-sub-ch (cons pid ch))
      ch))

  (define (sigchld-fiber)
    (define children (make-eqv-hashtable))
    (enable-signal SIGCHLD)
    (do () (#f)
      (match (perform-operation
              (choice-operation (wrap-operation (wait-signal-operation SIGCHLD)
                                                (lambda _ 'SIGCHLD))
                                (wrap-operation (get-operation chld-sub-ch)
                                                (lambda (x) (cons 'sub x)))))
        ['SIGCHLD
         (let* ((si (make-bytevector sizeof-siginfo_t 0))
                (ru (make-bytevector sizeof-rusage 0))
                (options (fxior WNOHANG WEXITED WUNTRACED WCONTINUED __WALL __WNOTHREAD))
                (status (let retry ()
                          (sys_waitid P_ALL -1 (bytevector-address si) options
                                      (bytevector-address ru)
                                      (lambda (errno)
                                        (cond ((eqv? errno EINTR) (retry))
                                              ((eqv? errno ECHILD) #f)
                                              (else
                                               (raise
                                                 (condition
                                                  (make-who-condition 'linux-listen-signals)
                                                  (make-syscall-error 'waitid errno))))))))))
           (when status
             (let ((pid (bytevector-u32-native-ref si offsetof-siginfo_t-si_pid))
                   (code (bytevector-s32-native-ref si offsetof-siginfo_t-si_code))
                   (uid (bytevector-u32-native-ref si offsetof-siginfo_t-si_uid))
                   (status (bytevector-s32-native-ref si offsetof-siginfo_t-si_status)))
               ;; Notify everyone who was interested in this child
               (let ((msg (list (cons 'status status) (cons 'pid pid)
                                (cons 'code code) (cons 'uid uid))))
                 (for-each (lambda (ch)
                             (spawn-fiber (lambda () (put-message ch msg))))
                           (hashtable-ref children pid '())))
               ;; The child is no more
               (when (or (eqv? code CLD_EXITED) (eqv? code CLD_KILLED) (eqv? code CLD_DUMPED))
                 (hashtable-delete! children pid)))))
         (acknowledge-signal SIGCHLD)]
        [('sub . (pid . ch))
         (hashtable-update! children pid (lambda (ch*) (cons ch ch*)) '())])))

  (spawn-fiber sigchld-fiber))
