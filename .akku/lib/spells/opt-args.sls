#!r6rs #!r6rs
;;; opt-args.sls --- Optional arguments

;; Copyright (C) 2001-2011, 2015, several authors -- see below.

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

;;@ Optional and named arguments.
(library (spells opt-args)
  (export define*
          lambda*
          
          define/named-args
          define/optional-args
          let-optionals*
          :optional)
  (import (rnrs)
          (for (srfi :8 receive) expand))


;; Copyright (C) 2010, 2011 Andreas Rottmann.
;;
;;@ A partial implementation of
;; @uref{http://mumble.net/~campbell/proposals/optional.text, Taylor
;; Campbell's ``optional'' proposal}.  All that is missing is support
;; for rest arguments.
(define-syntax lambda*
  (lambda (stx)
    (define (split-bindings bindings)
      (let loop ((bindings bindings)
                 (identifiers '())
                 (optional-identifiers '())
                 (optional-values '())
                 (presence-identifiers '()))
        (define (found-optional optional value presence)
          (loop (cdr bindings)
                identifiers
                (cons optional optional-identifiers)
                (cons value optional-values)
                (cons presence presence-identifiers)))
        (if (null? bindings)
            (values (reverse identifiers)
                    optional-identifiers
                    optional-values
                    presence-identifiers)
            (syntax-case (car bindings) ()
              ((optional value presence)
               (found-optional #'optional #'value #'presence))
              ((optional value)
               (found-optional #'optional #'value #f))
              (id
               (and (identifier? #'id) (null? optional-identifiers))
               (loop (cdr bindings)
                     (cons #'id identifiers)
                     optional-identifiers
                     optional-values
                     presence-identifiers))))))
    (define (generate-clauses ids optional-ids optional-values presence-ids)
      (let loop ((optional-ids optional-ids)
                 (used-values '())
                 (optional-values optional-values)
                 (clauses '()))
        (if (null? optional-ids)
            (cons (generate-clause ids optional-ids presence-ids used-values)
                  clauses)
            (loop (cdr optional-ids)
                  (cons (car optional-values) used-values)
                  (cdr optional-values)
                  (cons (generate-clause ids
                                         optional-ids
                                         presence-ids
                                         used-values)
                        clauses)))))
    (define (generate-clause ids optional-ids presence-ids used-values)
      (with-syntax (((id ...) ids)
                    ((optional-id ...) (reverse optional-ids))
                    ((used-value ...) used-values)
                    ((presence-id ...) (remv #f presence-ids))
                    ((presence-value ...)
                     (create-presence-values presence-ids (length used-values))))
        #'((id ... optional-id ...)
           (let ((presence-id presence-value) ...)
             (f id ... optional-id ... used-value ... presence-id ...)))))
    (define (create-presence-values presence-ids n-used-values)
      (let loop ((i 0) (presence-ids presence-ids) (result '()))
        (let ((present? (>= i n-used-values)))
          (cond ((null? presence-ids)
                 (reverse result))
                ((car presence-ids)
                 (loop (+ i 1) (cdr presence-ids) (cons present? result)))
                (else
                 (loop (+ i 1) (cdr presence-ids) result))))))
    (syntax-case stx ()
      ((_ (binding ...) . body)
       (receive (ids optional-ids optional-values presence-ids)
                (split-bindings #'(binding ...))
         (let ((clauses (generate-clauses ids optional-ids optional-values presence-ids)))
           (with-syntax (((id ...) ids)
                         ((optional-id ...) (reverse optional-ids))
                         ((presence-id ...) (remv #f presence-ids))
                         ((clause ...) clauses))
             #'(let ((f (lambda (id ... optional-id ... presence-id ...)
                          . body)))
                 (case-lambda
                   clause ...)))))))))

(define-syntax define*
  (syntax-rules ()
    ((define* (id binding ...) . body)
     (define id (lambda* (binding ...) . body)))))


;; The following code is taken from scsh, file scsh/let-opt.scm.
;;
;; Copyright (c) 2001 Olin Shivers, BSD license.
;;

;;@ Bind arguments from an argument rest-list to variables.
;;
;; Typical usage is like this:
;; @lisp
;;   (define (foo arg1 arg2 . args)
;;     (let-optionals* args ((opt1 'default1) (opt2 'default2))
;;       ...))
;; @end lisp
(define-syntax let-optionals*
  (syntax-rules ()
    ((let-optionals* arg (opt-clause ...) body ...)
     (let ((rest arg))
       (%let-optionals* rest (opt-clause ...) body ...)))))

;;@stop

(define-syntax %let-optionals*
  (syntax-rules ()
    ((%let-optionals* arg (((var ...) xparser) opt-clause ...) body ...)
     (call-with-values (lambda () (xparser arg))
       (lambda (rest var ...)
         (%let-optionals* rest (opt-clause ...) body ...))))
    
    ((%let-optionals* arg ((var default) opt-clause ...) body ...)
     (call-with-values (lambda () (if (null? arg) (values default '())
				      (values (car arg) (cdr arg))))
       (lambda (var rest)
	 (%let-optionals* rest (opt-clause ...) body ...))))

    ((%let-optionals* arg ((var default test) opt-clause ...) body ...)
     (call-with-values (lambda ()
			 (if (null? arg) (values default '())
			     (let ((var (car arg)))
			       (if test (values var (cdr arg))
				   (error "arg failed LET-OPT test" var)))))
       (lambda (var rest)
	 (%let-optionals* rest (opt-clause ...) body ...))))

    ((%let-optionals* arg ((var default test supplied?) opt-clause ...) body ...)
     (call-with-values (lambda ()
			 (if (null? arg) (values default #f '())
			     (let ((var (car arg)))
			       (if test (values var #t (cdr arg))
				   (error "arg failed LET-OPT test" var)))))
       (lambda (var supplied? rest)
	 (%let-optionals* rest (opt-clause ...) body ...))))

    ((%let-optionals* arg (rest) body ...)
     (let ((rest arg)) body ...))

    ((%let-optionals* arg () body ...)
     (if (null? arg) (let () body ...)
	 (error "Too many arguments in let-opt" arg)))))

;;@defspec :optional rest-arg default-exp [test-pred]
;;
;; This form is for evaluating optional arguments and their defaults
;; in simple procedures that take a @emph{single} optional
;; argument. It is a macro so that the default will not be computed
;; unless it is needed.
;; 
;; @var{rest-arg} is a rest list from a lambda -- e.g., @var{R} in
;;     (lambda (a b . r) ...)
;;
;;@itemize @bullet
;; @item
;; If @var{rest-arg} has 0 elements, evaluate @var{default-exp} and
;; return that.
;; @item
;; If @var{rest-arg} has 1 element, return that element.
;; @item
;; If @var{rest-arg} has >1 element, error.
;;@end itemize
;;
;; If there is an @var{test-pred} form, it is a predicate that is used
;; to test a non-default value. If the predicate returns false, an
;; error is raised.
;;
;;@end defspec
(define-syntax :optional
  (syntax-rules ()
    ((:optional rest default-exp)
     (let ((maybe-arg rest))
       (if (pair? maybe-arg)
	   (if (null? (cdr maybe-arg)) (car maybe-arg)
	       (error "too many optional arguments" maybe-arg))
	   default-exp)))

    ((:optional rest default-exp arg-test)
     (let ((maybe-arg rest))
       (if (pair? maybe-arg)
	   (if (null? (cdr maybe-arg))
	       (let ((val (car maybe-arg)))
		 (if (arg-test val) val
		     (error "Optional argument failed test"
			    'arg-test val)))
	       (error "too many optional arguments" maybe-arg))
	   default-exp)))))

;;@stop

;; Copyright (c) Jose Antonio Ortega Ruiz <jao@gnu.org>
;; Start date: Sat Oct 23, 2004 00:27

;; auxiliar syntax
(define-syntax args->alist
  (syntax-rules ()
    ((_ ((label? value?) ...)) (list (cons 'label? value?) ...))
    ((_ x? ...) (error "unexpected arguments format" 'x? ...))))


;;@ macro making it easy to define functions that take named
;; arguments of the form @code{(name (label value) (label2 value2) ...)}
;;
;; it can be used as:
;; @example
;;    (define/named-args (name (label default) ...) <forms or procedure>)
;; @end example
(define-syntax define/named-args
  (syntax-rules ()
    ((_ (name? (arg? def?) ...) (fun? aa? ...))
     (define/named-args
       (name? (arg? def?) ...)
       (lambda (arg? ...) (fun? aa? ...)) "define/named-args"))
    ((_ (name? (arg? def?) ...) proc?)
     (define/named-args
       (name? (arg? def?) ...) proc? "define/named-args"))
    ((_ (name? (arg? def?) ...) proc? "define/named-args")
     (define-syntax name?
       (syntax-rules ()
         ((_ . aa?)
          (let ((aalist (args->alist aa?)))
            (let ((arg? (cond ((null? aalist) def?)
                              ((assq 'arg? aalist) => cdr)
                              (else def?)))...)
              (proc? arg? ...)))))))
    ((_ (name? (arg? def?) ...) form1? form2? rest? ...)
     (define/named-args (name? (arg? def?) ...)
       (lambda (arg? ...) form1? form2? rest? ...) "define/named-args"))))

;;@ Macro that allows for simple definition of functions with
;; optional arguments.
;;
;; it can be used as:
;; @example
;;   (define/optional-args (name (arg ... (optional (optarg default) ...)))
;;     expr ...)
;; @end example
(define-syntax define/optional-args
  (syntax-rules (optional)
    ((_ (name . bindings) . bodies)
      (define/optional-args "seek-optional" bindings () ((name . bindings) . bodies)))

    ((_ "seek-optional" ((optional . _opt-bindings))
       (reqd ...) ((name . _bindings) . _bodies))
      (define (name reqd ... . _rest)
        (let-optionals* _rest _opt-bindings
                        . _bodies)))

    ((_ "seek-optional" (x . rest) (reqd ...) form)
     (define/optional-args "seek-optional" rest (reqd ... x) form))
    
    ((_ "seek-optional" not-a-pair reqd form)
     (define . form))                 ; no optional found, regular define
    
    ((_ name body)                    ; just the definition for 'name',
     (define name body))              ; for compatibilibility with define
    ))

)
