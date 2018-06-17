#!r6rs #!r6rs
;;; define-values.sls --- define-values syntax.

;; Copyright (C) 2009, 2011 Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

;;@ Define multiple identifiers using the results of a single
;; expression.
(library (spells define-values)
  (export define-values)
  (import (for (rnrs base) run expand)
          (for (rnrs syntax-case) run expand))

  ;;@defspec define-values (name ...) body ...
  ;;
  ;; Defines the identifiers given by @var{name} @dots{} by using the
  ;; values returned by @var{body}.
  ;;
  ;;@end defspec
  (define-syntax define-values
    (lambda (form)
      ;; The temporaries generated for `dummy' are just a workaround
      ;; for a psyntax bug in Guile.
      (syntax-case form ()
        ((_ () exp ...)
         (with-syntax (((dummy) (generate-temporaries '(dummy))))
           (syntax
            (define dummy (begin exp ... 'dummy)))))
	((_ (id ...) exp0 exp ...)
	 ;; Mutable-ids are needed so that ids defined by
	 ;; define-values can be exported from a library (mutated
	 ;; variables cannot be exported).  This fix is due to Andre
	 ;; van Tonder.
	 (with-syntax (((mutable-id ...) (generate-temporaries (syntax (id ...))))
		       ((result ...)     (generate-temporaries (syntax (id ...))))
                       ((dummy)          (generate-temporaries '(dummy))))
	   (syntax
	    (begin
	      (define mutable-id) ...
	      (define dummy
		(call-with-values
                    (lambda () exp0 exp ...)
		  (lambda (result ...)
		    (set! mutable-id result) ...
                    'dummy)))
	      (define id mutable-id) ...))))))))
