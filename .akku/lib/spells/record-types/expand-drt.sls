#!r6rs #!r6rs
;;; expand-drt.sls --- Macro expanders for `define-record-type*'

;; Copyright (C) 2009, 2011, 2015 Andreas Rottmann <a.rottmann@gmx.at>

;; R6RS syntax-case port of Taylor Campbell's Public
;; Domain `define-record-type*' macro expander:
;; <http://mumble.net/~campbell/scheme/defrectype*.scm>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(library (spells record-types expand-drt)
  (export expand-define-record-type*
          expand-define-functional-fields)
  (import (for (rnrs base) run (meta -1))
          (rnrs syntax-case)
          (only (srfi :1 lists) append-map)
          (for (srfi :8 receive) run (meta -1))
          (for (srfi :9 records) (meta -1))
          (srfi :39 parameters)
          (spells syntax-utils))

(define (expand-define-record-type* stx)
  ((call-with-current-continuation
    (lambda (lose)
      (lambda ()
        (parameterize (($lose (lambda (message subform)
                                (lose
                                 (lambda ()
                                   (syntax-violation message stx subform))))))
          (syntax-case stx ()
            ((k type-name (conser-name conser-args ...) (other-fields ...))
             (receive (needs-conser-layer? arg-tags vars inits)
                      (compute-vars+inits #'(conser-args ...)
                                          #'(other-fields ...))
               (with-syntax (((real-conser)
                              (if needs-conser-layer?
                                  (generate-temporaries '(real-conser))
                                  #'(conser-name))))
                 #`(begin
                     (define-record-type type-name
                       (real-conser #,@arg-tags)
                       #,(identifier-append #'k #'type-name '?)
                       #,@(generate-field-specs #'k
                                                #'(conser-args ...)
                                                #'(other-fields ...)
                                                #'type-name))
                     #,@(if needs-conser-layer?
                            #`((define (conser-name #,@vars)
                                 (real-conser #,@inits)))
                            #'()))))))))))))

(define $lose (make-parameter #f))
(define (lose msg subform) (($lose) msg subform))

(define (compute-vars+inits conser-args other-fields)
  (let ((vars (reverse-map
               (lambda (x)
                 (syntax-case x ()
                   ((var) (identifier? #'var) #'var)
                   (var (identifier? #'var) #'var)
                   (_ (lose "invalid maker argument specifier" x))))
               conser-args)))
    (let loop ((fields other-fields)
               (needs-conser-layer? #f)
               (arg-tags vars)
               (inits vars))
      (if (null? fields)
          (values needs-conser-layer?
                  (reverse arg-tags)
                  (reverse vars)
                  (reverse inits))
          (let ((field (car fields)))
            (syntax-case field ()
              (id
               (identifier? #'id)
               (loop (cdr fields)
                     needs-conser-layer?
                     arg-tags
                     inits))
              ((id init)
               (identifier? #'id)
               (loop (cdr fields)
                         #t
                         (cons #'id arg-tags)
                         (cons #'init inits)))
              (_
               (lose "invalid field specifier" field))))))))

(define (reverse-map proc list)
  (let loop ((list list) (tail '()))
    (if (null? list)
        tail
        (loop (cdr list) (cons (proc (car list)) tail)))))

(define (generate-field-specs k conser-args other-fields type-name)
  (append (map (lambda (x)
                 (receive (tag set?)
                          (syntax-case x ()
                            ((tag)      (values #'tag #t))
                            (tag        (values #'tag #f)))
                   (with-syntax ((tag tag))
                     #`(tag #,(make-field-accessor k
                                                   type-name
                                                   #'tag)
                            #,@(if set?
                                   (list (make-field-setter
                                          k
                                          type-name
                                          #'tag))
                                   '())))))
               conser-args)
          (map (lambda (x)
                 (with-syntax ((tag (syntax-case x ()
                                      ((tag init) #'tag)
                                      (tag        #'tag))))
                   #`(tag #,(make-field-accessor k type-name #'tag)
                          #,(make-field-setter   k type-name #'tag))))
               other-fields)))

(define (make-field-accessor k type-name tag)
  (identifier-append k type-name '- tag))

(define (make-field-setter k type-name tag)
  (identifier-append k 'set- type-name '- tag '!))

(define (make-field-modifier k type-name tag)
  (identifier-append k type-name "-modify-" tag))

(define (make-field-replacer k type-name tag)
  (identifier-append k type-name "-with-" tag))

(define (make-field-default k type-name tag)
  (identifier-append k type-name "-default-" tag))

(define (expand-define-functional-fields stx)
  (syntax-case stx ()
    ((k type-name fields ...)
     (with-syntax ((unconser (identifier-append #'k #'type-name "-components"))
                   (conser (identifier-append #'k "make-" #'type-name)))
       #`(begin
           (define (unconser obj)
             (values #,@(map (lambda (f)
                               #`(#,(make-field-accessor #'k #'type-name f) obj))
                             #'(fields ...))))
           #,@(append-map
               (lambda (field)
                 (list
                  #`(define (#,(make-field-replacer #'k #'type-name field)
                             obj value)
                      (receive (fields ...) (unconser obj)
                        (conser #,@(map (lambda (f)
                                          (if (free-identifier=? f field)
                                              #'value
                                              f))
                                        #'(fields ...)))))
                  #`(define (#,(make-field-modifier #'k #'type-name field)
                             obj modifier)
                      (#,(make-field-replacer #'k #'type-name field)
                       obj (modifier (#,(make-field-accessor #'k #'type-name field)
                                      obj))))
                  #`(define (#,(make-field-default #'k #'type-name field)
                             obj value)
                      (#,(make-field-modifier #'k #'type-name field)
                       obj (lambda (v)
                             (or v value))))))
               #'(fields ...)))))))

)
