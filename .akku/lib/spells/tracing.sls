#!r6rs #!r6rs
;;; tracing.sls --- Trace procedure invocations.

;; Copyright (C) 2009, 2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

;;@ Trace procedures for debugging.
(library (spells tracing)
  (export trace-define trace-lambda trace-procedure)
  (import (rnrs base)
          (spells tracing compat))

;;@defspec trace-lambda
;; @lisp
;; (trace-lambda @var{label} @var{arguments}
;;   @var{body} @dots{})@end lisp
;;@end defspec

;;@defspec trace-define
;; @lisp
;; (trace-define (@var{name} . @var{arguments})
;;   @var{body} @dots{})@end lisp
;;@end defspec


;;@defspec trace-procedure
;; @lisp
;; (trace-procedure @var{label} @var{procedure})@end lisp
;;
;; Returns a wrapper procedure that traces calls to @var{procedure},
;; which must be an expression that evaluates to a procedure.  As with
;; @code{trace-lambda}, @var{label} is an identifier for use in the
;; trace output.
;;
;;@end defspec

  (define-syntax trace-procedure
    (syntax-rules ()
      ((trace-procedure <label> <proc-expr>)
       (let ((proc <proc-expr>))
         (trace-lambda <label> args
           (apply proc args))))))

  )
