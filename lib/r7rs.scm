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
    r7include-expr r7include-original-expr
    r7condexp? r7condexp-clause*
    r7condexp-clause? r7condexp-clause-feature-req r7condexp-clause-declaration*
    r7unknown? r7unknown-declaration
    r7lib-referenced-features
    r7lib-implementation-names
    r7lib-has-generic-implementation?
    r7condexp-eval)
  (import
    (rnrs (6))
    (only (srfi :1 lists) append-map delete-duplicates)
    (only (wak fmt) wrt)
    (xitomatl AS-match)
    (akku lib schemedb)
    (akku private logging))

(define logger:akku.r7rs (make-logger logger:akku 'r7rs))
(define log/warn (make-fmt-log logger:akku.r7rs 'warning))

;; Convert an R7RS import set to an R6RS import set. Keep library names.
(define (r7rs-import-set->r6rs import-set)
  (define f
    (match-lambda
     [((and (or 'only 'except 'prefix 'rename) modifier) set^ x* ...)
      `(,modifier ,(f set^) ,@x*)]
     [('srfi (? number? x) . rest)
      ;; XXX: This is what chez-srfi uses, but there is also the (srfi
      ;; sN) convention in e.g. surfage.
      `(srfi ,(string->symbol (string-append ":" (number->string x))) . ,rest)]
     [((and (or 'for 'library) id) . x)
      ;; "for" and "library" are special in R6RS imports, wrap in (library ...)
      `(library ,(r7rs-library-name->r6rs (cons id x)))]
     [x
      (r7rs-library-name->r6rs x)]))
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
      ((or (r7include? decl) (r7begin? decl) (r7unknown? decl))
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
         (import ,@(map r7import->r6rs import*))
         ,@(append-map
            (lambda (decl)
              (cond ((r7include? decl)
                     (list (r7include-expr decl)))
                    ((r7unknown? decl)
                     (list (r7unknown-declaration decl)))
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
          ci?                           ;case-insensitive?
          original-expr))               ;original expression

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

(define (r7include-expr decl)
  (list (if (r7include-ci? decl) 'include-ci 'include)
        (r7include-target-filename decl)))

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
              (make-r7include filename fn (eq? incl 'include-ci) decl))
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
      ((and ('error (? string? message)) x) ;somewhat common extension
       (list (make-r7unknown x)))
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

;; Return the declarations in the first true clause; otherwise #f.
(define (r7condexp-eval decl feature-list library-installed?)
  (exists (lambda (clause)
            (and (eval-cond-expand (r7condexp-clause-feature-req clause)
                                   feature-list
                                   library-installed?)
                 (r7condexp-clause-declaration* clause)))
          (r7condexp-clause* decl))))
