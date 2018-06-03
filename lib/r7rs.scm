;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2018 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
#!r6rs

;; R7RS trickery.

(library (akku lib r7rs)
  (export
    r7rs-import-set->r6rs
    r7rs-library-name->r6rs
    r7rs-export-set->r6rs
    r7rs-library->r6rs-library
    parse-r7rs-define-library
    r7lib? r7lib-name r7lib-declaration*
    r7export? r7export-internal-name r7export-external-name
    r7import? r7import-name r7import-set
    r7begin? r7begin-body*
    r7include? r7include-source-filename r7include-target-filename r7include-ci?
    r7condexp? r7condexp-clause*
    r7condexp-clause? r7condexp-clause-feature-req r7condexp-clause-declaration*
    r7unknown? r7unknown-declaration

    r7lib-referenced-features
    r7lib-implementation-names
    r7lib-has-generic-implementation?
    r7condexp-eval
    )
  (import
    (rnrs (6))
    (only (srfi :1 lists) append-map delete-duplicates)
    (only (wak fmt) wrt)
    (xitomatl AS-match)
    (akku lib schemedb)
    (akku private logging))

(define logger:akku.r7rs (make-logger logger:akku 'r7rs))
(define log/warn (make-fmt-log logger:akku.r7rs 'warning))

;; Convert an R7RS import set to an R6RS import set.
(define (r7rs-import-set->r6rs import-set)
  (define f
    (match-lambda
     [((and (or 'only 'except 'prefix 'rename) modifier) set^ x* ...)
      `(,modifier ,(f set^) ,@x*)]
     ;; FIXME: This is a giant mess. Did yuni do something about this?
     [('scheme 'base)
      '(only (rnrs)
             *                        +
             -                        ...
             /                        <
             <=                       =
             =>                       >
             >=
             abs                      and
             append                   apply
             assoc                    assq
             assv                     begin
             binary-port?             boolean=?
             boolean?                 #;bytevector
             #;bytevector-append        bytevector-copy
             bytevector-copy!         bytevector-length
             bytevector-u8-ref        bytevector-u8-set!
             bytevector?              caar
             cadr
             call-with-current-continuation
             call-with-port           call-with-values
             call/cc                  car
             case                     cdar
             cddr                     cdr
             ceiling                  char->integer
             #;char-ready?              char<=?
             char<?                   char=?
             char>=?                  char>?
             char?                    close-input-port
             close-output-port        close-port
             complex?                 cond
             #;cond-expand              cons
             current-error-port       current-input-port
             current-output-port      define
             define-record-type       define-syntax
             #;define-values            denominator
             do                       dynamic-wind
             else                   eof-object
             eof-object?            eq?
             equal?                 eqv?
             error                  #;error-object-irritants
             #;error-object-message   #;error-object?
             even?                  exact
             exact-integer-sqrt     #;exact-integer?
             exact?                 expt
             #;features               #;file-error?
             floor                  #;floor-quotient
             #;floor-remainder        ;#floor/
             flush-output-port      for-each
             gcd                    #;get-output-bytevector
             #;get-output-string      guard
             if                     #;include
             #;include-ci             inexact
             inexact?               #;input-port-open?
             input-port?            integer->char
             integer?               lambda
             lcm                    length
             let                    let*
             let*-values            let-syntax
             let-values             letrec
             letrec*                letrec-syntax
             list                   list->string
             list->vector           #;list-copy
             list-ref               #;list-set!
             #;list-tail              list?
             make-bytevector        #;make-list
             #;make-parameter         make-string
             make-vector            map
             max                    member
             memq                   memv
             min                    #;modulo
             negative?              newline
             not                    null?
             number->string         number?
             numerator              odd?
             #;open-input-bytevector  #;open-input-string
             #;open-output-bytevector #;open-output-string
             or                     #;output-port-open?
             output-port?           pair?
             #;parameterize           #;peek-char
             #;peek-u8                port?
             positive?              procedure?
             quasiquote             quote
             #;quotient               raise
             raise-continuable      rational?
             rationalize            #;read-bytevector
             #;read-bytevector!       #;read-char
             #;read-error?            #;read-line
             #;read-string            #;read-u8
             real?                  #;remainder
             reverse                round
             set!                   #;set-car!
             #;set-cdr!               #;square
             string                 string->list
             string->number         string->symbol
             string->utf8           #;string->vector
             string-append          string-copy
             #;string-copy!           #;string-fill!
             string-for-each        string-length
             #;string-map             string-ref
             #;string-set!            string<=?
             string<?               string=?
             string>=?              string>?
             string?                substring
             symbol->string         symbol=?
             symbol?                #;syntax-error
             syntax-rules           textual-port?
             ;; truncate               truncate-quotient
             ;; truncate-remainder     truncate/
             #;u8-ready?              unless
             unquote                unquote-splicing
             utf8->string           values
             vector                 vector->list
             #;vector->string         #;vector-append
             #;vector-copy            #;vector-copy!
             vector-fill!           vector-for-each
             vector-length          vector-map
             vector-ref             vector-set!
             vector?                when
             with-exception-handler #;write-bytevector
             #;write-char             #;write-string
             #;write-u8               zero?)]
     [('scheme 'case-lambda)
      '(only (rnrs) case-lambda)]
     [('scheme 'complex)
      '(only (rnrs)
             angle            imag-part
             magnitude        make-polar
             make-rectangular real-part)]
     [('scheme 'eval)
      ;; TODO: intercept these and translate imports?
      '(rnrs eval)]
     [('scheme 'file)
      '(only (rnrs)
             call-with-input-file   call-with-output-file
             delete-file            file-exists?
             open-binary-input-file open-binary-output-file
             open-input-file        open-output-file
             with-input-from-file   with-output-to-file)]
     [('scheme 'inexact)
      '(only (rnrs)
             acos      asin
             atan      cos
             exp       finite?
             infinite? log
             nan?      sin
             sqrt      tan)]
     [('scheme 'lazy)
      ;; XXX: hmm.
      '(only (rnrs)
             delay    #;delay-force
             force    #;make-promise
             #;promise?)]
     [('scheme 'load)
      ;; XXX: greater hmm.
      '(only (chezscheme)
             load)]
     [('scheme 'process-context)
      '(only (rnrs)
             command-line              #;emergency-exit
             exit
             #;get-environment-variable
             #;get-environment-variables)]
     [('scheme 'read)
      ;; XXX: should use (laesare reader).
      '(only (rnrs) read)]
     [('scheme 'repl)
      ;; XXX: greater hmm again.
      '(only (chezscheme) interaction-environment)]
     [('scheme 'time)
      '(only (rnrs)
             ;; current-jiffy      current-second
             ;; jiffies-per-second
             )]

     [('scheme 'write)
      '(only (rnrs)
             display      write
             #;write-shared #;write-simple)]
     [('scheme 'r5rs)
      '(only (rnrs)
             *                        +
             -                        ...
             /                        <
             <=                       =
             =>                       >
             >=
             abs                      acos
             and                      angle
             append                   apply
             asin                     assoc
             assq                     assv
             atan                     begin
             boolean?                 caaaar
             caaadr                   caaar
             caadar                   caaddr
             caadr                    caar
             cadaar                   cadadr
             cadar                    caddar
             cadddr                   caddr
             cadr
             call-with-current-continuation
             call-with-input-file     call-with-output-file
             call-with-values         car
             case                     cdaaar
             cdaadr                   cdaar
             cdadar                   cdaddr
             cdadr                    cdar
             cddaar                   cddadr
             cddar                    cdddar
             cddddr                   cdddr
             cddr                     cdr
             ceiling                  char->integer
             char-alphabetic?         char-ci<=?
             char-ci<?                char-ci=?
             char-ci>=?               char-ci>?
             char-downcase            char-lower-case?
             char-numeric?            char-ready?
             char-upcase              char-upper-case?
             char-whitespace?         char<=?
             char<?                   char=?
             char>=?                  char>?
             char?                    close-input-port
             close-output-port        complex?
             cond                     cons
             cos                      current-input-port
             current-output-port      define
             define-syntax            delay
             denominator              display
             do                       dynamic-wind
             else                     eof-object?
             eq?                      equal?
             eqv?                     eval
             even?                    exact->inexact
             exact?                   exp
             expt                     floor
             for-each                 force
             gcd                      if
             imag-part                inexact->exact
             inexact?                 input-port?
             integer->char            integer?
             interaction-environment lambda
             lcm                       length
             let                       let*
             let-syntax                letrec
             letrec-syntax             list
             list->string              list->vector
             list-ref                  list-tail
             list?                     load
             log                       magnitude
             make-polar                make-rectangular
             make-string               make-vector
             map                       max
             member                    memq
             memv                      min
             modulo                    negative?
             newline                   not
             null-environment          null?
             number->string            number?
             numerator                 odd?
             open-input-file           open-output-file
             or                        output-port?
             pair?                     peek-char
             positive?                 procedure?
             quasiquote                quote
             quotient                  rational?
             rationalize               read
             read-char                 real-part
             real?                     remainder
             reverse                   round
             scheme-report-environment
             set!                      set-car!
             set-cdr!                  sin
             sqrt                      string
             string->list              string->number
             string->symbol            string-append
             string-ci<=?              string-ci<?
             string-ci=?               string-ci>=?
             string-ci>?               string-copy
             string-fill!              string-length
             string-ref                string-set!
             string<=?                 string<?
             string=?                  string>=?
             string>?                  string?
             substring                 symbol->string
             symbol?                   syntax-rules
             tan                       truncate
             values                    vector
             vector->list              vector-fill!
             vector-length             vector-ref
             vector-set!               vector?
             with-input-from-file      with-output-to-file
             write                     write-char
             zero?

             )]
     [('scheme 'cxr)
      '(only (rnrs)
             caaaar caaadr caaar caadar caaddr caadr
             cadaar cadadr cadar caddar cadddr caddr
             cdaaar cdaadr cdaar cdadar cdaddr cdadr
             cddaar cddadr cddar cdddar cddddr cdddr)]
     [('scheme 'char)
      '(only (rnrs)
             char-alphabetic? char-ci<=?
             char-ci<?        char-ci=?
             char-ci>=?       char-ci>?
             char-downcase    char-foldcase
             char-lower-case? char-numeric?
             char-upcase      char-upper-case?
             char-whitespace? ;; digit-value
             string-ci<=?     string-ci<?
             string-ci=?      string-ci>=?
             string-ci>?      string-downcase
             string-foldcase  string-upcase)]
     [('srfi (? number? x))
      ;; This is what chez-srfi uses, but there is also the (srfi sN)
      ;; convention in e.g. surfage.
      `(srfi ,(string->symbol (string-append ":" (number->string x))))]
     [((and (or 'for 'library) id) . x)
      ;; for and library are special in R6RS imports, wrap in (library ...)
      `(library (,id . ,x))]
     [x x]))
  (f import-set))

;; R7RS library names can contain numbers; R6RS library names can't.
(define (r7rs-library-name->r6rs library-name)
  (map (lambda (id)
         (if (integer? id)
             (string->symbol (string-append ":" (number->string id))) ;bad choice?
             id))
       library-name))

;; Convert an R7RS export set to an R6RS export set.
(define (r7rs-export-set->r6rs export-set)
  (define f
    (match-lambda
     [('rename from to)
      `(rename (,from ,to))]
     [x x]))
  (f export-set))

;; This procedure converts the library to R6RS. The idea is that
;; define-library can be replaced by library, etc and things will
;; mostly work. Not everything of course, it will be a journey.
;; Self-quoted vectors will not work when it's done this way.
(define (r7rs-library->r6rs-library def-lib filename read-include implementation-name)
  (define (parse-library-declaration decl import* export* body*)
    (cond
      ((or (r7include? decl) (r7begin? decl))
       (values import* export* (cons decl body*)))
      ((r7export? decl)
       (values import* (cons decl export*) body*))
      ((r7import? decl)
       (values (cons decl import*) export* body*))
      ((r7condexp? decl)
       (cond ((r7condexp-eval decl (implementation-features implementation-name)
                              (lambda (lib-name)
                                ;; FIXME: Should really know about all
                                ;; libraries in the packages.
                                (or (r7rs-builtin-library? lib-name implementation-name)
                                    (r6rs-builtin-library? lib-name implementation-name)
                                    (match lib-name
                                      [('srfi . _) #t]
                                      [else #f]))))
              => (lambda (new-decl*)
                   (parse-decls new-decl* import* export* body*)))
             (else
              (log/warn "No cond-expand clause matches in " (wrt filename)
                        " for the implementation " implementation-name)
              (values import* export* body*))))
      (else
       (values import* export* body*))))
  (define (parse-decls decl* import* export* body*)
    (let lp ((decl* decl*))
      (if (null? decl*)
          (values import* export* body*)
          (let-values (((import* export* body*) (lp (cdr decl*))))
            (parse-library-declaration (car decl*) import* export* body*)))))
  (define (r7export->r6rs decl)
    (let ((int (r7export-internal-name decl))
          (ext (r7export-external-name decl)))
      (if (eq? int ext)
          int
          `(rename (,int ,ext)))))
  (define (r7import->r6rs decl)
    (r7rs-import-set->r6rs (r7import-set decl)))

  (let ((lib (parse-r7rs-define-library def-lib filename read-include)))
    (let-values (((import* export* body*) (parse-decls (r7lib-declaration* lib) '() '() '())))
      `(library ,(r7rs-library-name->r6rs (r7lib-name lib))
         (export ,@(map r7export->r6rs export*))
         (import ,@(map r7import->r6rs import*)
                 (only (rnrs) syntax-case syntax))
         ;; (define %akku-source-filename% ,filename)
         (define-syntax cond-expand
           (lambda (x)
             (syntax-case x (else)
               [(_ (else body ...))
                #'(begin body ...)]
               [(_ (feature-req body ...)
                   (feature-req* body* ...) ...)
                #'(cond-expand (feature-req* body* ...) ...)])))
         ,@(append-map
            (lambda (decl)
              (cond ((r7include? decl)
                     (read-include (r7include-source-filename decl)
                                   (r7include-target-filename decl)
                                   ;; TODO:
                                   #;(r7include-ci? decl)))
                    (else
                     (r7begin-body* decl))))
                body*)))))

;;; R7RS define-library record representation

(define-record-type r7lib
  (sealed #t)
  (fields name                          ;library name
          declaration*))                ;library declarations

(define-record-type r7export
  (sealed #t)
  (fields internal-name                 ;name inside library
          external-name))               ;name outside library

(define-record-type r7import
  (sealed #t)
  (fields name                          ;library name
          set))                         ;original import set

(define-record-type r7begin
  (sealed #t)
  (fields body*))                       ;commands or definitions

(define-record-type r7include
  (sealed #t)
  (fields source-filename
          target-filename
          ci?))                         ;case-insensitive?

(define-record-type r7condexp
  (sealed #t)
  (fields clause*))                     ;cond-expand clauses

(define-record-type r7condexp-clause
  (sealed #t)
  (fields feature-req                   ;feature requirements
          declaration*))                ;library declarations

(define-record-type r7unknown
  (sealed #t)
  (fields declaration))

;; Recordizes an R7RS library declaration.
(define (parse-r7rs-define-library def-lib filename read-include)
  (define import-set-library-name
    (match-lambda
     [('only set _id* ...) (import-set-library-name set)]
     [('except set _id* ...) (import-set-library-name set)]
     [('prefix set _id ...) (import-set-library-name set)]
     [('rename set (_old-id* _new-id*) ...) (import-set-library-name set)]
     [(id* ... ((? number?) ...)) id*]  ;Sagittarius can mix in r6rs syntax
     [lib-name lib-name]))
  (define (parse-decl filename decl depth)
    (unless (list? decl)
      (assertion-violation 'parse-r7rs-define-library
                           "Found non-list datum in a library declaration" decl))
    ;; XXX: Some of these patterns are more permissive than required.
    (match decl
      (((and (or 'include 'include-ci) incl) (? string? fn*) ...)
       (map (lambda (fn)
              (make-r7include filename fn (eq? incl 'include-ci)))
            fn*))
      (('include-library-declarations (? string? fn*) ...)
       (when (> depth 100)
         (assertion-violation 'parse-r7rs-define-library
                              "Included library declarations are nested too deeply" fn*))
       (unless (procedure? read-include)
         (assertion-violation 'parse-r7rs-define-library
                              "No read-include procedure was provided"))
       (append-map (lambda (fn)
                     (append-map (lambda (decl)
                                   (parse-decl fn decl (+ depth 1)))
                                 (read-include filename fn)))
                   fn*))
      (('export export-spec* ...)
       (map (match-lambda
             [('rename internal-name external-name)
              (make-r7export internal-name external-name)]
             [(? symbol? name)
              (make-r7export name name)])
            export-spec*))
      (('import import-set* ...)
       (map (lambda (import-set)
              (make-r7import (import-set-library-name import-set)
                             import-set))
            import-set*))
      (('cond-expand (feature-req* decl** ...) ...)
       (list
        (make-r7condexp
         (let f ((feature-req* feature-req*) (decl** decl**))
           (if (null? feature-req*)
               '()
               (let ((feature-req (car feature-req*))
                     (decl* (car decl**)))
                 (cons (make-r7condexp-clause
                        (if (and (eq? feature-req 'else)
                                 (null? (cdr feature-req*)))
                            '(and)      ;simplify else to (and)
                            feature-req)
                        (append-map (lambda (decl)
                                      (parse-decl filename decl depth))
                                    decl*))
                       (f (cdr feature-req*) (cdr decl**)))))))))
      (('begin body* ...)
       (list (make-r7begin body*)))
      (x
       (log/warn "Unknown declaration in R7RS library: " (wrt x))
       (list (make-r7unknown x)))))
  (match def-lib
    [('define-library lib-name . decl*)
     (make-r7lib lib-name (append-map (lambda (decl)
                                        (parse-decl filename decl 0))
                                      decl*))]))

;;; Analysis of recordized R7RS libraries.

;; Get all referenced feature names and libraries from cond-expand clauses.
(define (r7lib-referenced-features lib)
  (define flatten-feature-req
    (match-lambda
     [(? symbol? feature) (list feature)]
     [('library (? list? library-name)) (list `(library ,library-name))]
     [('and req* ...) (append-map flatten-feature-req req*)]
     [('or req* ...) (append-map flatten-feature-req req*)]
     [('not req) (flatten-feature-req req)]
     [x '()]))
  (define (extract decl)
    (cond
      ((r7condexp? decl)
       (append-map
        (lambda (clause)
          (append (flatten-feature-req (r7condexp-clause-feature-req clause))
                  (append-map extract (r7condexp-clause-declaration* clause))))
        (r7condexp-clause* decl)))
      (else '())))
  (delete-duplicates (append-map extract (r7lib-declaration* lib))
                     equal?))

;; Get all implementation names that show up in cond-expand feature
;; requirements.
(define (r7lib-implementation-names lib)
  (filter rnrs-implementation-name? (r7lib-referenced-features lib)))

(define (eval-cond-expand feature-req feature-list library-installed?)
  (define ev
    (match-lambda
     [(? symbol? feature) (memq feature feature-list)]
     [('library (? list? library-name)) (library-installed? library-name)]
     [('and req* ...) (for-all ev req*)]
     [('or req* ...) (exists ev req*)]
     [('not req) (not (ev req))]
     [x #f]))
  (ev feature-req))

;; #t if the library can be imported by any R7RS implementation (not
;; just specific implementations).
(define (r7lib-has-generic-implementation? lib)
  (define (walk decl)
    (cond
      ((r7condexp? decl)
       (exists
        (lambda (clause)
          (and (eval-cond-expand (r7condexp-clause-feature-req clause)
                                 (implementation-features #f)
                                 (lambda (lib-name)
                                   ;; XXX: Should preferably know
                                   ;; about all libraries in the
                                   ;; packages.
                                   (or (r7rs-builtin-library? lib-name #f)
                                       (r6rs-builtin-library? lib-name #f)
                                       (match lib-name
                                         (('srfi . _) #t)
                                         (else #f)))))
               (for-all walk (r7condexp-clause-declaration* clause))))
        (r7condexp-clause* decl)))
      (else #t)))
  (for-all walk (r7lib-declaration* lib)))

;; Return the declarations in the first true clause; otherwise #f;
(define (r7condexp-eval decl feature-list library-installed?)
  (exists (lambda (clause)
            (and (eval-cond-expand (r7condexp-clause-feature-req clause)
                                   feature-list
                                   library-installed?)
                 (r7condexp-clause-declaration* clause)))
          (r7condexp-clause* decl)))

)
