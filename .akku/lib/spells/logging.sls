#!r6rs #!r6rs
;;; logging.sls --- Logging library.

;; Copyright (C) 2009, 2010, 2011, 2015 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

;;@ This library provides procedures implementing a flexible logging
;; system, for example useful for error reporting and logging debug
;; messages.
(library (spells logging)
  (export root-logger
          make-logger
          make-log
          
          log-entry?
          log-entry-level
          log-entry-level-name
          log-entry-object
          log-entry-logger-name

          default-log-formatter

          let-logger-properties
          set-logger-properties!)
  (import (rnrs base)
          (rnrs control)
          (rnrs mutable-pairs)
          (rnrs io simple)
          (only (rnrs io ports) flush-output-port)
          (except (srfi :1 lists) map for-each)
          (srfi :19 time)
          (srfi :39 parameters)
          (spells alist)
          (spells match)
          (spells opt-args)
          (spells record-types))

;;@extractors (import (spells private stexidoc)) spells-extractors

;; @subheading Introduction
;;
;; This library is heavily inspired by Python's
;; @uref{http://docs.python.org/library/logging.html,logging
;; module}. The main concepts involved are:
;;
;; @table @emph
;;
;; @item Log levels
;;
;; Each item to be logged is issued at a specific log level, which is
;; a numeric value that designates is significance. The library
;; provides convinient ways to filter out items below a certain
;; threshold.
;;
;; @item Log entries
;; A log entry is an object encapsulating a reference to the logger
;; (see below) where it originated, a log level, the time when issued, and the
;; item to be logged, an application-defined object.
;;
;; @item Log handlers
;;
;; A log handler is a single-argument procedure that is
;; responsible for somehow "presenting" the log message, which may
;; involve arbitary action, for example sending it over the network to
;; some log host.
;;
;; @item Loggers
;;
;; Loggers consult a threshold and a list of filters to decide whether
;; to process a log entry that is handed to them. Matching log entries
;; are then delegated to log handler procedures, each of which is
;; again guarded by a threshold and a list of filters.
;;
;; Each logger has name in a hierarchical namespace; therefore, each
;; logger, except for the root logger, has a parent, which it will
;; pass log entries to if propagation is enabled for that logger.
;;
;; @end table
;;
;; Loggers are intended to be the static part of the logging system in
;; your application: typically, you will define one for each part of
;; the application you want to be able to differentiate with regards
;; to logging. Their associated handlers, the thresholds and filters,
;; need not be static to the code or a single run of the application,
;; making it possible to change logging behaviour mid-run, which is
;; useful especially for long-running processes.
;;
;;@stop


;;; Data types

(define-record-type* logger
  (%make-logger parent name properties)
  ())

(define-record-type* logger-prop
  (make-logger-prop threshold propagate? filters handlers)
  ())

(define (make-property-accessor accessor)
  (lambda (logger)
    (accessor ((logger-properties logger)))))

(define logger-threshold (make-property-accessor logger-prop-threshold))
(define logger-propagate? (make-property-accessor logger-prop-propagate?))
(define logger-filters (make-property-accessor logger-prop-filters))
(define logger-handlers (make-property-accessor logger-prop-handlers))

(define (make-default-properties)
  (make-parameter (make-logger-prop #f #t '() '())))

(define-record-type* log-handler
  (%make-log-handler proc threshold filters)
  ())

(define* (make-log-handler proc (threshold #f) (filters '()))
  (%make-log-handler proc (numeric-level threshold) filters))

;;@ Log entry object.
(define-record-type* log-entry
  (make-log-entry logger level time object)
  ())

(define (log-entry-level-name entry)
  (let ((level (log-entry-level entry)))
    (or (any (lambda (entry)
               (and (= (cdr entry) level) (car entry)))
             *builtin-levels*)
        level)))

(define (log-entry-logger-name entry)
  (logger-name (log-entry-logger entry)))

;;; Internal functions

(define (passes? threshold filters entry)
  (and (or (eqv? threshold #f)
           (>= (log-entry-level entry) threshold))
       (every (lambda (filter) (filter entry)) filters)))

(define (handle handler entry)
  (if (passes? (log-handler-threshold handler)
               (log-handler-filters handler)
               entry)
      ((log-handler-proc handler) entry)))

(define (do-log logger entry)
  (define (do-handle)
    (for-each (lambda (handler) (handle handler entry))
              (logger-handlers logger)))
  
  (let ((threshold (logger-threshold logger))
        (parent (logger-parent logger)))
    (if (passes? threshold (logger-filters logger) entry)
        (cond ((eqv? parent #f) ;; root logger?
               (do-handle))
              (else
               (do-handle)
               (if (logger-propagate? logger)
                   (do-log parent entry)))))))

;;; Builtin handlers & formatters

;;@ A simple log formatter.
;;
;; Output the log entry @var{entry} on @var{port} in the following
;; way:
;;
;; @example
;; "(" @var{N0} ("." @var{Ni})* ": [" @var{LEVEL} "] " @var{OBJ} ")"
;; @end example
;;
;; In the above template @var{N0} and the @var{Ni} are substituted by
;; the first and further name components (respectively), @var{LEVEL}
;; is either the symbolic name (for builtin levels), or the numeric
;; value. @var{OBJ} is the output produced by either applying
;; @ref{r5rs display} to the log entry's object and @var{port}, or, if
;; the log object is a procedure, applying it to @var{port}.
;;
(define (default-log-formatter entry port)
  (display #\( port)
  (do ((name (logger-name (log-entry-logger entry)) (cdr name)))
      ((null? name))
    (display (car name) port)
    (if (not (null? (cdr name)))
        (display #\. port)))
  (display ": [" port)
  (display (log-entry-level-name entry)  port)
  (display "] " port)
  (let ((obj (log-entry-object entry)))
    (if (procedure? obj)
        (obj port)
        (display obj port)))
  (display #\) port)
  (newline port)
  (flush-output-port port))

;;; Global state

(define root-logger (%make-logger #f '() (make-default-properties)))

(define (default-logger-properties)
  `((handlers (lambda (entry)
                (default-log-formatter entry (current-output-port))))))

(define *builtin-levels* '((trace    .  5000)
                           (debug    . 10000)
                           (info     . 20000)
                           (warning  . 30000)
                           (error    . 40000)
                           (critical . 50000)))

(define (numeric-level level)
  (cond ((and (symbol? level)
              (assq level *builtin-levels*))
         => cdr)
        ((or (eqv? level #f) (integer? level))
         level)
        (else
         (error 'numeric-level "invalid level" level))))

(define (property-ref config key default)
  (cond ((assq key config) => cdr)
        (else default)))

(define (property-ref* config key default)
  (cond ((assq key config) => cadr)
        (else default)))

;;; Public interface

;;@ Create a logger object.
;;
(define (make-logger parent name)
  (%make-logger parent
                (append (logger-name parent) (list name))
                (make-default-properties)))

;;@ Create a logging procedure.
;;
;; @var{logger} must be a logger, on which entries issued via the
;; returned procedure will be issued.
;;
;; If @var{level} is supplied, it specifies the logging level at which
;; the returned procedure will issue its log entrys; it may be either
;; an integer, or one of the builtin levels @code{'debug},
;; @code{'info}, @code{'warning}, @code{'error}, @code{'critical}. The
;; returned procedure will accept a single argument, the object to be
;; logged.
;;
;; If @var{level} is not supplied, the returned procedure will accept
;; two arguments, the logging level (as would be supplied for
;; @var{level}), and the object to be logged.
;;
(define make-log
  (case-lambda
    ((logger level)
     (let ((level (numeric-level level)))
       (lambda (obj)
         (do-log logger (make-log-entry logger level (current-time) obj)))))
    ((logger)
     (lambda (level obj)
       (do-log logger (make-log-entry logger
                                      (numeric-level level)
                                      (current-time)
                                      obj))))))


;;@ Set the properties of @var{logger} according to the alist
;; @var{properties}, which may contain the following keys:
;;
;; @table @code
;; @item handlers
;;
;; A list of handlers specifications. Each item in this list may
;; either be a procedure, or a list of one of these forms:
;;
;; @table @code
;; @item (@var{threshold} @var{handler})
;; @item (@var{threshold} (@var{filter} ...) @var{handler})
;; @item (@var{handler})
;; @end table
;;
;; Each @var{handler} and @var{filter} may be either a procedure of
;; the appropriate type, or a symbol, referencing a (procedure) value
;; in the alist @var{procedures}. It may also be a list of the form
;; @code{(@var{name} . @var{args})}, which is interpreted by looking
;; up @var{name} in @var{procedures}, and applying the resulting
;; procedure to @var{args}, the result from that application is used
;; for the corresponding handler or filter procedure. This simple
;; lookup facility is provided so that applications can use a value
;; taken from a configuration file for @var{properties}, and supply
;; the necessary handlers and filters (or their constructors)
;; statically via @var{procedures}.
;;
;; If either of these keys is not present, the empty list is used as
;; default.
;;
;; @item threshold
;; A logging level, or @code{#f}. All entries with levels below (using @code{<}
;; as comparison procedure) the threshold will be discarded (@code{#f}, which is
;; the default, is considered to be negative infinity, hence allowing all entries
;; to pass).
;; @item propagate?
;; Whether to propage the message to parent handlers. Defaults to @code{#t}.
;; @end table
;;
;; The values for both @code{threshold} and @code{propagate?} are
;; expected to be in the second (i.e. @code{cadr} position) of the
;; corresponding entry, so you don't have to, and must not, use dots
;; to force a single cons cell.
;;
;; If @var{properties} is not specified, a default value is used,
;; which contains just a @code{handler} list with a single handler,
;; without threshold and filters, using @ref{default-log-formatter} to
;; format the log entry.
;;
(define* (set-logger-properties! logger
                                 (properties (default-logger-properties))
                                 (procedures '()))
  
  ((logger-properties logger) (alist->logger-prop properties procedures)))

(define (alist->logger-prop properties procedures)
  (define (proc-ref name)
    (or (assq-ref procedures name)
        (error 'configure-logger "invalid procedure reference" name)))
  (define (make-proc spec)
    (cond ((procedure? spec)
           spec)
          ((symbol? spec)
           (proc-ref spec))
          ((pair? spec)
           (apply (proc-ref (car spec)) (cdr spec)))))
  (define (make-handler spec)
    (match spec
           ((threshold proc)
            (make-log-handler (make-proc proc) threshold))
           ((threshold (filters ___) proc)
            (make-log-handler (make-proc proc) threshold (map make-proc filters)))
           ((proc)
            (make-log-handler (make-proc proc)))
           ((? procedure? proc)
            (make-log-handler proc))))
  (make-logger-prop (numeric-level
                      (property-ref* properties 'threshold #f))
                     (property-ref* properties 'propagate? #t)
                     (property-ref properties 'filters '())
                     (map make-handler
                          (property-ref properties 'handlers '()))))

;;@ Change logger properties for the dynamic extent of
;; @var{body}.
;;
;; This macro works similiar to a @code{parameterize} form; the
;; left-hand side of the bindings must evaluate to logger objects, and
;; the right-hand-side must evaluate to a logger property alist as
;; described in @xref{spells logging
;; set-logger-properties!,set-logger-properties!}.
(define-syntax let-logger-properties
  (syntax-rules ()
    ((let-logger-properties ((logger properties) ...) body ...)
     (parameterize (((logger-properties logger)
                     (alist->logger-prop properties '()))
                    ...)
       body ...))))


;; Must go last, since it may expand into an expression
(define-record-discloser logger
  (lambda (l)
    (list 'logger (logger-name l))))

)
