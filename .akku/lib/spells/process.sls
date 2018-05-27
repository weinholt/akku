#!r6rs #!r6rs
;;; process.sls --- OS process interface.

;; Copyright (C) 2009-2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

;;@ An interface to operating system processes.  This library defines
;; a disjoint data type for asynchronous processes, procedures to
;; spawn them and wait for their termination as well as several
;; utilities built upon these primitives.
(library (spells process)
  (export process?
          process-id
          process-input
          process-output
          process-errors

          spawn-process
          wait-for-process
          close-process-ports

          call-with-process-input
          call-with-process-output
          call-with-process
          
          run-process
          run-process/string
          run-process/lines
          run-process/sexps

          run-shell-command

          get-process-id
          )
  (import (except (rnrs base) string-copy string-for-each string->list)
          (rnrs io ports)
          (rnrs io simple)
          (srfi :8 receive)
          (except (srfi :1 lists) map for-each)
          (srfi :13 strings)
          (spells pathname)
          (spells delimited-readers)
          (spells process compat))

;; The following documents the interface provided by the `(spells
;; process compat)' library.

;;;@subsection Process data type

;; Asynchronous processes are represented by objects of a disjoint
;; type.  A process as an identifier, which is usually a positive
;; integer provided by the operating system, as well as a set of ports
;; that correspond to its standard input, output and error.

;;@defun process? object
;; Returns @code{#t} if @var{object} is a process, and @code{#f}
;; otherwise.
;;@end defun

;;@defun process-id process
;; Returns the identifier of @var{process}.
;;@end defun

;;@defun process-input process
;;@defunx process-output process
;;@defunx process-errors process
;;
;; Returns ports corresponding to the standard input, output and error
;; of @var{process}, respecively.  If, upon spawning, the process has
;; been supplied with ports to be associated to its standard I/O
;; ports, the corresponding procedure will return @code{#f} instead of
;; a port.
;;
;;@end defun

;;;@subsection Spawning and waiting for termination

;;@defun spawn-process env stdin stdout stderr prog . args
;;
;; Run @var{prog} with the environment @var{env} and arguments
;; @var{args} asynchronously. The return value is a process object,
;; which can be passed to @ref{wait-for-process},
;; @ref{close-process-ports}, @ref{process-input},
;; @ref{process-output}.
;;
;; The parameters @var{stdin}, @var{stdout} and @var{stderr} can each
;; either be @code{#f}, or a binary port.  If any of these is
;; @code{#f}, the field in the process object contains a port that is
;; redirected to the spawned process's standard input, standard
;; output, or standard error, respectively.  If a port is given as any
;; of these arguments, the corresponding port in the process object
;; will be @code{#f}, and instead the port given as argument will be
;; used for the spawned process's standard standard input, standard
;; output, or standard error, respectively.  It is
;; implementation-dependent which ports qualify as
;; @code{spawn-process} arguments, but usually all binary ports that
;; directly refer to operating system resources, such as files, pipes
;; or network connections can be used.
;;
;;@end defun

;;@defun wait-for-process process
;;
;; Wait for termination of @var{process}, which must have been created
;; by @ref{spawn-process}. The return values are the exit status and
;; the terminating signal (in case the process has been terminated by
;; a signal, or false otherwise).
;;
;;@end defun

;;;@subsection Utilities

;;@ Close the input, output and error port associated with
;; @var{process}.
(define (close-process-ports process)
  (let ((input (process-input process))
        (output (process-output process))
        (errors (process-errors process)))
    (if input (close-port input))
    (if output (close-port output))
    (if errors (close-port errors))))


;;@ Apply @var{proc} to @var{process}.  After @var{proc} has returned,
;; @var{process}' ports are closed and its termination is awaited.
;;
;; The procedure returns the termination status and terminating
;; signal, plus any values returned by @var{proc}.
(define (call-with-process process proc)
  (receive results (proc process)
    (close-process-ports process)
    (receive (status signal) (wait-for-process process)
      (apply values status signal results))))

;;@ Run @2 with the environment @1 and arguments @3 synchronously (@0
;; returns after the process has terminated, yielding the the exit
;; status, and the terminating signal (if the process has been
;; terminated by a signal, or false otherwise).
(define (run-process env prog . args)
  (wait-for-process 
   (apply spawn-process
          env
          (standard-input-port)
          (standard-output-port)
          (standard-error-port)
          prog args)))

;;@ Run @2, which must be a list of the executable name and any
;; arguments with the environment @1.
;;
;; The procedure of one argument @3 is passed an output port which
;; corresponds to the standard input of the process. @3 and the
;; process execute at the same time; when @3 returns, @0 waits for the
;; process to terminate and returns the exit status, terminating
;; signal (if the process has been terminated by a signal, or false
;; otherwise) and the values returned by @3.
(define (call-with-process-input env prog+args receiver)
  (let* ((process (apply spawn-process
                         env
                         #f
                         (standard-output-port)
                         (standard-error-port)
                         prog+args))
         (port (transcoded-port (process-input process) (native-transcoder))))
    (receive results (receiver port)
      (close-port port)
      (close-process-ports process)
      (receive status+signal (wait-for-process process)
        (apply values (append status+signal results))))))

;;@ Run @2, which must be a list of the executable name and any
;; arguments with the environment @1.
;;
;; The procedure of one argument @3 is passed an input port which
;; corresponds to the standard output of the process. @3 and the
;; process execute at the same time; when @3 returns, @0 waits for the
;; process to terminate and returns the exit status, terminating
;; signal (if the process has been terminated by a signal, or false
;; otherwise) and the values returned by @3.
;;
;; This means, given a procedure @code{port->lines},
;; @ref{run-process/lines} can be implemented like this:
;; @lisp
;; (define (run-process/lines env prog . args)
;;   (call-with-process-output env (cons prog args)
;;     port->lines))
;; @end lisp
(define (call-with-process-output env prog+args receiver)
  (let* ((process (apply spawn-process
                         env
                         (standard-input-port)
                         #f
                         (standard-error-port)
                         prog+args))
         (port (transcoded-port (process-output process) (native-transcoder))))
    (receive results (receiver port)
      (close-process-ports process)
      (receive status+signal (wait-for-process process)
        (apply values (append status+signal results))))))

;;@ Run @2 with the environment @1 and arguments @3 synchronously (@0
;; returns after the process has terminated). The return values are
;; the exit status, terminating signal (if the process has been
;; terminated by a signal) and the standard output captured in a
;; string, list of strings or s-expressions, respectively.
;;
;; Using @code{srfi-8}, @0 can be used as follows:
;; @lisp
;; (receive (status signal output)
;;          (run-process/string #f "cat /etc/hostname")
;;   ...)
;; @end lisp
(define (run-process/string env prog . args)
  (call-with-process-output env (cons prog args)
    (lambda (output)
      (string-unfold eof-object?
                     values
                     (lambda (seed)
                       (read-char output))
                     (read-char output)))))

(define (run-process/lines env prog . args)
  (call-with-process-output env (cons prog args)
    port->lines))

(define (run-process/sexps env prog . args)
  (call-with-process-output env (cons prog args)
    port->sexps))

;;@stop
(define (port->lines port)
  (unfold eof-object? values (lambda (seed) (read-line port)) (read-line port)))

(define (port->sexps port)
  (unfold eof-object? values (lambda (seed) (read port)) (read port)))

)
