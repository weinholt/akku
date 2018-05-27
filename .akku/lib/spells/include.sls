#!r6rs #!r6rs
;;; include.sls --- Include scheme source code.

;; Copyright (C) 2009, 2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

;;@ File inclusion of Scheme code.
(library (spells include)
  (export include-file
          include-file/downcase)
  (import (rnrs)
          (for (only (spells include compat)
                     annotation?
                     annotation-expression) expand)
          (for (spells include helpers) expand))

  ;;@defspec include-file
  ;; @lisp
  ;; (include-file ((@var{directory-component} @dots{}) @var{file-component}))@end lisp
  ;;
  ;; Include a the contents of the file specified by the
  ;; @var{directory-component}s and @var{file-component} at in the
  ;; lexical context of the @code{include-file} form; i.e.  the
  ;; effect is the same as if the contents of the specified file
  ;; (which must be syntactically valid Scheme code) was present
  ;; instead of the @code{include-file} form.
  ;;
  ;;@end defspec
  (define-syntax include-file
    (lambda (stx)
      (syntax-case stx ()
        ((k <path>)
         (include-file/aux 'include-file #'k (syntax->datum #'<path>) values)))))

  ;;@defspec include-file/downcase
  ;;
  ;; This macro has the same syntax and behavior as
  ;; @code{include-file}, but applies case folding to all symbols
  ;; appearing in included file's content.  This behavior is
  ;; especially useful for including R5RS code which exploits the
  ;; case-insensitivity of R5RS.
  ;;
  ;;@end defspec
  (define-syntax include-file/downcase
    (lambda (stx)
      ;; This loses all the annotations, but Ikarus provides no way to
      ;; (re-)construct annotation objects ATM.
      (define (downcase thing)
        (let ((form (if (annotation? thing)
                        (annotation-expression thing)
                        thing)))
          (cond ((symbol? form)
                 (string->symbol (string-downcase (symbol->string form))))
                ((pair? form)
                 (cons (downcase (car form))
                       (downcase (cdr form))))
                (else
                 thing))))
      (syntax-case stx ()
        ((k <path>)
         (include-file/aux 'include-file #'k (syntax->datum #'<path>) downcase)))))
  
)
