;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2017-2018 Göran Weinholt <goran@weinholt.se>
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

;; Scheme source parser.

(library (akku lib file-parser)
  (export
    examine-source-file examine-other-file fmt-artifact
    artifact? artifact-path artifact-path-list artifact-form-index
    artifact-last-form? artifact-imports artifact-assets
    artifact-implementation artifact-for-test? artifact-internal?
    artifact-for-bin? artifact-directory artifact-filename
    make-generic-file generic-file?
    make-legal-notice-file legal-notice-file?
    r6rs-library? r6rs-library-name r6rs-library-version r6rs-library-exports
    r6rs-library-original-name
    r6rs-program?
    r7rs-library? r7rs-library-name r7rs-library-exports
    r7rs-program?
    module? module-name
    library-reference? library-reference-name library-reference-version-reference
    library-reference-original-import-spec
    library-reference-satisfied?
    include-reference? include-reference-path include-reference-realpath
    include-reference-conversion include-reference-original-include-spec
    include-reference-read-all read-all-forms)
  (import
    (rnrs (6))
    (only (srfi :1 lists) delete-duplicates)
    (srfi :115 regexp)
    (laesare reader)
    (wak fmt)
    (xitomatl AS-match)
    (akku lib r7rs)
    (akku lib schemedb)
    (akku lib utils)
    (akku private logging))

(define logger:akku.file-parser (make-logger logger:akku 'file-parser))
(define log/info (make-fmt-log logger:akku.file-parser 'info))
(define log/warn (make-fmt-log logger:akku.file-parser 'warning))
(define log/debug (make-fmt-log logger:akku.file-parser 'debug))
(define log/trace (make-fmt-log logger:akku.file-parser 'trace))

(define rx-legal-notice-filename     ;relative to the root of the repo
  (rx (w/nocase
       (or (: (or "AUTHORS" "CREDITS" "CONTRIBUTORS"
                  "DISCLAIMERS" "NOTICE" "THANKS")
              (* (~ "/")))
           "debian/copyright"
           ;; https://reuse.software/practices/
           (: (or "LICENSE" "LICENCE" "COPYING" "COPYRIGHT") (* (~ "/")))
           (: "LICENSES/" (* any))))))

(define-record-type artifact
  (fields path path-list form-index last-form? imports assets implementation))

(define-record-type generic-file
  (parent artifact)
  (sealed #t)
  (nongenerative)
  (protocol
   (lambda (p)
     (lambda (path path-list)
       (let ((make (p path path-list 0 #t '() '() #f)))
         (make))))))

(define-record-type legal-notice-file   ;copyright notices etc
  (parent artifact)
  (sealed #t)
  (nongenerative)
  (protocol
   (lambda (p)
     (lambda (path path-list)
       (let ((make (p path path-list 0 #t '() '() #f)))
         (make))))))

(define-record-type r6rs-library
  (parent artifact)
  (sealed #t)
  (nongenerative)
  (fields name version exports
          original-name))               ;#f or unmangled name

(define-record-type r6rs-program
  (parent artifact)
  (sealed #t)
  (nongenerative))

(define-record-type r7rs-library
  (parent artifact)
  (sealed #t)
  (nongenerative)
  (fields name exports))

(define-record-type r7rs-program
  (parent artifact)
  (sealed #t)
  (nongenerative))

(define-record-type module
  (parent artifact)
  (sealed #t)
  (nongenerative)
  (fields name))

(define-record-type library-reference
  (fields name version-reference original-import-spec)
  (sealed #t)
  (nongenerative))

(define-record-type include-reference
  (fields path                          ;path from libpath root
          realpath                      ;real path
          conversion                    ;#f, foldcase, downcase
          original-include-spec)
  (sealed #t)
  (nongenerative))

;; Get a list of forms.
(define (read-all-forms filename tolerant?)
  (call-with-input-file filename
    (lambda (port)
      (let ((reader (make-reader port filename)))
        (reader-tolerant?-set! reader tolerant?)
        (let lp ((datum* '()))
          (let ((datum (read-datum reader)))
            (if (eof-object? datum)
                (reverse datum*)
                (lp (cons datum datum*)))))))))

;; Read all forms in an include-reference.
(define (include-reference-read-all include-ref)
  ;; FIXME: handle case folding.
  (read-all-forms (include-reference-realpath include-ref) #t))

(define (artifact-directory artifact)
  (match (string-split (artifact-path artifact) #\/)
    ((fn) "")
    ((dir* ... _fn) (fold-left path-join (car dir*) (cdr dir*)))))

(define (artifact-filename artifact)
  (match (string-split (artifact-path artifact) #\/)
    ((fn) fn)
    ((_dir* ... fn) fn)))

;; Does the file appear to be part of a test suite?
(define (artifact-for-test? artifact)
  (or (memq 'test (artifact-path-list artifact))
      (memq 'tests (artifact-path-list artifact))
      (match (string-split (artifact-path artifact) #\.)
        ((base . _)
         (or (string-suffix? "-tests" base)
             (string-suffix? "-test" base)
             (string-suffix? "/test" base)
             (string-prefix? "test-" base)))
        (else #f))))

;; Does the file appear to be internal/private to the package?
(define (artifact-internal? artifact)
  (or (memq 'private (artifact-path-list artifact))
      (memq 'internal (artifact-path-list artifact))))

;; Does the file appear to be headed for bin/?
(define (artifact-for-bin? artifact)
  (or (memq 'bin (artifact-path-list artifact))
      (memq 'demos (artifact-path-list artifact))
      (memq 'programs (artifact-path-list artifact))))

(define (fmt-artifact a)
  (define (fmt-library-reference ref)
    (cat
     (if (null? (library-reference-version-reference ref))
         (wrt (library-reference-name ref))
         (cat "{name: " (wrt (library-reference-name ref)) ", version: "
              (wrt (library-reference-version-reference ref)) "}"))))
  (define (fmt-include-reference ref)
    (cat
     "{path: " (wrt (include-reference-path ref))
     ", realpath: " (wrt (include-reference-realpath ref))
     (cond ((include-reference-conversion ref) =>
            (lambda (conv) (cat ", conversion: " conv)))
           (else fmt-null))
     "}"))
  (cat
   "Filename: " (wrt (artifact-path a))
   (if (> (artifact-form-index a) 0)
       (cat fl "Form index: " (artifact-form-index a))
       fmt-null)
   fl "Type: "
   (cond ((r6rs-library? a) "R6RS library")
         ((r6rs-program? a) "R6RS program")
         ((r7rs-library? a) "R7RS library")
         ((r7rs-program? a) "R7RS program")
         ((module? a) "Module")
         ((legal-notice-file? a) "Notice")
         ((generic-file? a) "Generic file")
         (else "Artifact"))

   (if (artifact-for-test? a) (cat fl "Is for test: true") fmt-null)
   (if (artifact-for-bin? a) (cat fl "Is for bin: true") fmt-null)
   (if (artifact-internal? a) (cat fl "Is internal: true") fmt-null)

   (if (artifact-implementation a)
       (cat fl "For implementation: "(artifact-implementation a))
       fmt-null)

   (cond ((r6rs-library? a)
          (cat fl "Name: "  (wrt (r6rs-library-name a))
               (if (null? (r6rs-library-version a))
                   fmt-null
                   (cat " version: " (wrt (r6rs-library-version a))))
               fl "Exports: "
               (fmt-join (lambda (exp) (cat fl "- " exp))
                         (r6rs-library-exports a))))
         ((r7rs-library? a)
          (cat fl "Name: "  (wrt (r7rs-library-name a))
               fl "Exports: "
               (fmt-join (lambda (exp) (cat fl "- " exp))
                         (r7rs-library-exports a))))
         ((module? a)
          (if (module-name a)
              (cat fl "Name: " (wrt (module-name a)))
              fmt-null))
         (else fmt-null))

   (cond ((pair? (artifact-imports a))
          (cat fl "Imports: "
               (fmt-join (lambda (imp)
                           (cat fl "- " (fmt-library-reference imp) nl))
                         (artifact-imports a))))
         (else fmt-null))

   (cond ((pair? (artifact-assets a))
          (cat fl "Assets: "
               (fmt-join (lambda (file-include)
                           (cat fl "- " (fmt-include-reference file-include) nl))
                         (artifact-assets a))))
         (else fmt-null))
   fl "---" fl))

;; Does the artifact satisfy the library reference?
(define (library-reference-satisfied? import artifact)
  (and (r6rs-library? artifact)
       ;; TODO: Check the version reference.
       (equal? (library-reference-name import)
               (r6rs-library-name artifact))))

;; Scan a file for includes, to see which files belong together.
(define (scan-for-includes/r6rs form realpath)
  ;; XXX: Note that this implementation can find includes which are in
  ;; quoted datums or contexts where the include form is not bound,
  ;; but this is not terribly important.
  (define (str/sym? x) (or (string? x) (symbol? x)))
  ;; Make the path to the included file, from the root of a directory
  ;; in the library search path.
  (define (make-path dir fn ext)
    (call-with-string-output-port
      (lambda (p)
        (for-each (lambda (component)
                    (display component p)
                    (display #\/ p))
                  dir)
        (display fn p)
        (when ext
          (display ext p)))))
  ;; Find the real path to the included file, from the package root.
  (define (make-pkgpath dir fn ext)     ;FIXME: rename
    ;; FIXME: This is vulnerable to path traversal attacks (files
    ;; outside the package may be checked).
    (let lp-root ((real-pkgroot (car (split-path realpath))))
      (let lp ((dir^ dir))
        (let ((pkgpath (path-join real-pkgroot (make-path dir^ fn ext))))
          (if (file-exists? pkgpath)
              pkgpath
              (if (null? dir^)
                  (cond ((member real-pkgroot '("" "."))
                         (unless (and (member dir '(() ("no" "where"))) ;oddness in xitomatl
                                      (equal? fn "doesnt-exist"))
                           (log/warn "Could not resolve include: " (list dir fn ext)))
                         #f)
                        (else
                         (lp-root (car (split-path real-pkgroot)))))
                  (lp (cdr dir^))))))))
  (match form
    ;; Implementations for R6RS. Paths are relative to the library path.
    ;; Derick Eddington (srfi and xitomatl)
    (('include/resolve ((? string? dir) ...) (? string? fn))
     (list (make-include-reference (make-path dir fn #f)
                                   (make-pkgpath dir fn #f)
                                   #f form)))
    ;; Andreas Rottmann (spells and wak)
    (((or 'include-file 'include-file/downcase) (((? str/sym? dir) ...) (? str/sym? fn)))
     (list (make-include-reference (make-path dir fn ".scm")
                                   (make-pkgpath dir fn ".scm")
                                   (if (eq? (car form) 'include-file) #f 'downcase)
                                   form)))
    (((or 'include-file 'include-file/downcase) ((? str/sym? dir) ...) (? str/sym? fn))
     (list (make-include-reference (make-path dir fn #f)
                                   (make-pkgpath dir fn #f)
                                   (if (eq? (car form) 'include-file) #f 'downcase)
                                   form)))
    ;; Chez Scheme
    (('include (? string? fn))
     (list (make-include-reference (make-path '() fn #f)
                                   (make-pkgpath '() fn #f)
                                   #f form)))
    (('quote _)
     '())
    ((? list? e*)
     (append-map (lambda (e)
                   (scan-for-includes/r6rs e realpath))
                 e*))
    (else '())))

;; Makes an include-reference for an R7RS-style include.
(define (mk-r7rs-include filename conversion original-include-spec from-lib-name from-realpath)
  (let ((target-path
         (resolve-pathname
          (call-with-string-output-port
            (lambda (p)
              ;; FIXME: Needs to do quoting of the filename per
              ;; whatever quoting the "r7rs" include will use in the
              ;; end.
              (do ((comp* from-lib-name (cdr comp*)))
                  ((null? (cdr comp*))
                   (put-string p filename))
                (display (car comp*) p)
                (put-char p #\/))))))
        (included-file-realpath
         (resolve-relative-filename from-realpath filename)))
    (make-include-reference target-path included-file-realpath
                            conversion original-include-spec)))

;; Scan an r7lib to extract imports, assets and exports. The filenames are
;; handled relative to the including file, although it is actually
;; implementation-specific and could be anything.
(define (parse-r7lib lib implementation-name)
  (define (parse-library-declaration decl import* include* export*)
    (cond
      ((r7include? decl)
       (log/debug "TODO: Scan R7RS include bodies for assets: "
                  (r7include-target-filename decl))
       (let ((incl (mk-r7rs-include (r7include-target-filename decl)
                                    (if (r7include-ci? decl) 'foldcase #f)
                                    (r7include-original-expr decl)
                                    (r7lib-name lib)
                                    (r7include-source-filename decl))))
         (values import*
                 (if (memp (lambda (x) (string=? (include-reference-path x)
                                                 (include-reference-path incl)))
                           include*)
                     include*                ;duplicate
                     (cons incl include*))
                 export*)))
      ((r7export? decl)
       (values import* include* (cons (r7export-external-name decl) export*)))
      ((r7import? decl)
       (values (cons (make-library-reference (r7import-name decl)
                                             '()
                                             (r7import-set decl))
                     import*)
               include* export*))
      ((r7condexp? decl)
       (let ((all-include*
              (if implementation-name
                  '()
                  (append-map
                   (lambda (clause)
                     ;; This is a generic parsing of the file and one
                     ;; of the includes from the other branches may be
                     ;; needed.
                     (let-values (((_i* include* _e*)
                                   (parse-decls (r7condexp-clause-declaration* clause)
                                                '() '() '())))
                       include*))
                   (r7condexp-clause* decl)))))
         (cond ((r7condexp-eval decl (implementation-features implementation-name)
                                (lambda (lib-name)
                                  ;; XXX: Should preferably know about
                                  ;; all libraries in the packages.
                                  (or (r7rs-builtin-library? lib-name implementation-name)
                                      (r6rs-builtin-library? lib-name implementation-name))))
                => (lambda (new-decl*)
                     (parse-decls new-decl* import* (append all-include* include*) export*)))
               (else
                (log/warn "No cond-expand clause matches in " (wrt (r7lib-name lib))
                          " for the implementation " implementation-name)
                (values import* (append all-include* include*) export*)))))
      (else
       (values import* include* export*))))
  (define (parse-decls decl* import* include* export*)
    (let lp ((decl* decl*))
      (if (null? decl*)
          (values import* include* export*)
          (let-values (((import* include* export*) (lp (cdr decl*))))
            (parse-library-declaration (car decl*) import* include* export*)))))
  (parse-decls (r7lib-declaration* lib) '() '() '()))

(define parse-r7rs-import-spec          ;XXX: also in (akku lib r7rs)
  (match-lambda
   (('only (? list? import-set) _id ...)
    (parse-r7rs-import-spec import-set))
   (('except (? list? import-set) _id ...)
    (parse-r7rs-import-spec import-set))
   (('prefix (? list? import-set) prefix)
    (identifier? #'prefix)
    (parse-r7rs-import-spec import-set))
   (('rename (? list? import-set) ((? symbol?) (? symbol?)) ...)
    (parse-r7rs-import-spec import-set))
   ((and ((not (or 'only 'except 'prefix 'rename)) . _)
         ((? (lambda (x) (or (symbol? x)
                             (and (number? x) (exact? x)
                                  (integer? x) (not (negative? x)))))
             id*) ...)
         spec)
    (make-library-reference id* '() spec))
   (x (assertion-violation 'parse-r7rs-import-spec
                           "Invalid r7rs import set" x))))

(define (parse-r6rs-import-spec spec)
  (define who 'parse-r6rs-import-spec)
  (define parse-import-set
    (match-lambda
     (('library (? list? library-reference))
      (parse-library-reference library-reference))
     (('only (? list? import-set) _id ...)
      (parse-import-set import-set))
     (('except (? list? import-set) _id ...)
      (parse-import-set import-set))
     (('prefix (? list? import-set) prefix)
      (identifier? #'prefix)
      (parse-import-set import-set))
     (('rename (? list? import-set) ((? symbol?) (? symbol?)) ...)
      (parse-import-set import-set))
     ((and ((not (or 'for 'library 'only 'except 'prefix 'rename)) . _)
           library-reference)
      (parse-library-reference library-reference))
     (x (assertion-violation who "Invalid import set" x))))
  (define parse-library-reference
    (match-lambda
     (((? symbol? id*) ... (and version-reference (? list?)))
      (make-library-reference id* (parse-version-reference version-reference) spec))
     (((? symbol? id*) ...)
      (make-library-reference id* '() spec))
     (x (assertion-violation who "Invalid library reference" x))))
  (define valid-subversion?
    (lambda (sv)
      (and (number? sv) (integer? sv) (exact? sv) (not (negative? sv)))))
  (define parse-sub-version-reference
    (match-lambda
     ((? valid-subversion? sub-version)
      sub-version)
     (('>= (? valid-subversion? sub-version))
      `(>= ,sub-version))
     (('<= (? valid-subversion? sub-version))
      `(<= ,sub-version))
     (('and sv* ...)
      `(and ,@(map parse-sub-version-reference sv*)))
     (('or sv* ...)
      `(or ,@(map parse-sub-version-reference sv*)))
     (('not sv)
      `(not ,(parse-sub-version-reference sv)))
     (x (assertion-violation who "Invalid sub-version reference" x))))
  (define parse-version-reference
    (match-lambda
     (('and v* ...)
      `(and ,@(map parse-version-reference v*)))
     (('or v* ...)
      `(or ,@(map parse-version-reference v*)))
     (('not v)
      `(not ,(parse-version-reference v)))
     ((sv sv* ...)
      `(,(parse-sub-version-reference sv) ,@(map parse-sub-version-reference sv*)))
     (x (assertion-violation who "Invalid version reference" x))))
  (match spec
    (('for import-set
           (or 'run 'expand
               ('meta (? (lambda (l)
                           (and (number? l) (exact? l) (integer? l))))))
           ...)
     (parse-import-set import-set))
    (import-set
     (parse-import-set import-set))))

(define parse-r6rs-export
  (match-lambda
   (('rename (local-names export-names) ...) export-names)
   ((? symbol? x) (list x))))

(define (path->implementation-name fn)
  (match (string-split fn #\.)
    ((_ impl (or "sls" "sps")) (string->symbol impl))
    (_ #f)))

;; Does this look like an R7RS import spec? Prefer false negatives.
;; Used to guess if a program is an R7RS or an R6RS program, if it
;; can't be determined by the lexical syntax.
(define (r7rs-import-set? import-set)
  (define (test-parse parser value parser-name)
    (guard (exn
            ((and (assertion-violation? exn)
                  (who-condition? exn)
                  (eq? (condition-who exn) parser-name))
             #f))
      (parser value)))
  (define (valid-r6rs-spec? spec)
    (test-parse parse-r6rs-import-spec spec 'parse-r6rs-import-spec))
  (define (valid-r7rs-spec? spec)
    (test-parse parse-r7rs-import-spec spec 'parse-r7rs-import-spec))
  (let ((is-valid-r6rs (for-all valid-r6rs-spec? import-set))
        (is-valid-r7rs (for-all valid-r7rs-spec? import-set)))
    (cond
      ((and is-valid-r7rs (not is-valid-r6rs)) #t)
      ((and is-valid-r6rs (not is-valid-r7rs)) #f)
      (else
       (let ((lib-name* (map (lambda (spec)
                               (library-reference-name (parse-r6rs-import-spec spec)))
                             import-set)))
         (cond ((exists (lambda (lib-name) (r6rs-builtin-library? lib-name #f)) lib-name*)
                #f)
               ((exists (lambda (lib-name)
                          (exists (lambda (implementation-name)
                                    (r7rs-builtin-library? lib-name implementation-name))
                                  r7rs-implementation-names))
                        lib-name*)
                #t)
               (else #f)))))))

;; Examine a source file and return a list of artifact record, or #f
;; if it's probably not source code.
(define (examine-source-file realpath path path-list)
  (define (maybe-library/module form form-index next-datum)
    (match form
      (('library (name+ver ...)             ;R6RS library
         ('export export* ...)
         ('import import* ...)
         . body*)
       (let* ((ver (find list? name+ver))
              (name (if ver (reverse (cdr (reverse name+ver))) name+ver)))
         (let* ((parsed-import-spec* (map parse-r6rs-import-spec import*))
                (implementation-name (or (path->implementation-name path)
                                         (r6rs-library-name*->implementation-name
                                          (map library-reference-name parsed-import-spec*))))
                (parsed-export* (append-map parse-r6rs-export export*)))
           (let ((include* (scan-for-includes/r6rs body* realpath)))
             (cons (make-r6rs-library path path-list form-index (eof-object? next-datum)
                                      parsed-import-spec* include*
                                      implementation-name name
                                      (or ver '()) parsed-export* #f)
                   ;; If an implementation requires libraries to be
                   ;; mangled then they show up as their own
                   ;; artifacts.
                   (append-map
                    (match-lambda
                     [(mangle-impl-name . mangled-lib-name)
                      (cond ((and implementation-name
                                  (not (eq? implementation-name mangle-impl-name)))
                             '())
                            ((or (memq mangle-impl-name
                                       (r6rs-library-block-for-implementations name))
                                 (memq mangle-impl-name
                                       (r6rs-library-omit-for-implementations name)))
                             (log/debug "Not installing " (wrt name) " for "
                                        mangle-impl-name)
                             '())
                            (else
                             (log/debug "Mangling " (wrt name) " to "
                                        (wrt mangled-lib-name) " for "
                                        mangle-impl-name)
                             (list (make-r6rs-library path path-list form-index
                                                      (eof-object? next-datum)
                                                      parsed-import-spec* include*
                                                      mangle-impl-name mangled-lib-name
                                                      (or ver '()) parsed-export* name))))])
                    (r6rs-library-name-mangle name)))))))
      (('define-library (name ...)      ;R7RS library
         . declaration*)
       ;; XXX: name can contain <identifier> or <uinteger 10> [sic!].
       (let* ((lib (parse-r7rs-define-library
                    form realpath
                    (lambda (source-filename target-filename)
                      (read-all-forms (resolve-relative-filename source-filename target-filename)
                                      #f))))
              (implementations (if (r7lib-has-generic-implementation? lib)
                                   (cons #f (r7lib-implementation-names lib))
                                   (r7lib-implementation-names lib))))
         (cond
           ((pair? implementations)
            (map (lambda (implementation-name)
                   (let-values (((import* include* export*)
                                 (parse-r7lib lib implementation-name)))
                     (make-r7rs-library path path-list form-index (eof-object? next-datum)
                                        import* include*
                                        (or implementation-name
                                            (r7rs-library-name*->implementation-name
                                             (map library-reference-name import*)))
                                        name export*)))
                 implementations))
           (else
            (log/debug "R7RS library is not loadable in any implementation: "
                       (wrt (r7lib-name lib)))
            #f))))
      (('define-module (name ...)
         . option*)
       ;; Guile module.
       (let ((imports (let lp ((option* option*))
                        (match option*
                          [('use-module interface-spec . rest*)
                           (cond ((match interface-spec
                                    [((? symbol? name*) ...)
                                     (make-library-reference name* '() interface-spec)]
                                    [(((? symbol? name*) ...) opt* ...)
                                     (let lp ((opt* opt*))
                                       (match opt*
                                         [('version (? list? ver))
                                          (make-library-reference name* ver interface-spec)]
                                         [(_ _ . rest*)
                                          (lp rest*)]
                                         [else
                                          (make-library-reference name* '() interface-spec)]))]
                                    [_ #f])
                                  => (lambda (lib-ref) (cons lib-ref (lp rest*))))
                                 (else
                                  (log/debug "Guile use-module not understood: " option*)
                                  (lp rest*)))]
                          [('autoload _ _ . rest*) (lp rest*)]
                          [('pure . rest*) (lp rest*)]
                          [(_ _ . rest*) (lp rest*)]
                          [else '()]))))
         (list (make-module path path-list form-index #t
                            imports '() 'guile name))))
      (('module (? symbol? name) (export* ...) body* ...)
       ;; Named Chez Scheme module. Imports and exports can appear
       ;; basically anywhere, making them very difficult to support
       ;; completely.
       (let ((import* '())
             (include* (scan-for-includes/r6rs body* realpath)))
         (list (make-module path path-list form-index (eof-object? next-datum)
                            import* include* 'chezscheme name))))
      (_                           ;some type of module or bare source
       (cond ((path->implementation-name path) =>
              (lambda (impl)
                (list (make-module path path-list form-index (eof-object? next-datum)
                                   #f '() impl #f))))
             (else
              ;; TODO: detect modules from various implementations
              #f)))))
  (define (maybe-program form form-index next-datum reader)
    (match form
      (('import import* ...)
       (cond
         ((r7rs-import-set? import*)
          ;; R7RS program.
          (let ((parsed-import-spec* (map parse-r7rs-import-spec import*))
                (include* '()))
            (log/debug "TODO: Scan R7RS program bodies for more imports/assets: " realpath)
            (list (make-r7rs-program path path-list form-index #t parsed-import-spec* include*
                                     (or (path->implementation-name path)
                                         (r7rs-library-name*->implementation-name
                                          (map library-reference-name parsed-import-spec*)))))))
         (else
          ;; R6RS program.
          (let ((parsed-import-spec* (map parse-r6rs-import-spec import*))
                (include* (let lp ((include* '()) (datum next-datum))
                            (if (eof-object? datum)
                                include*
                                (lp (append (scan-for-includes/r6rs datum realpath)
                                            include*)
                                    (read-datum reader))))))
            (list (make-r6rs-program path path-list form-index #t parsed-import-spec* include*
                                     (or (path->implementation-name path)
                                         (r6rs-library-name*->implementation-name
                                          (map library-reference-name parsed-import-spec*)))))))))
      (_ #f)))
  (log/trace "Examining source file: " (wrt realpath))
  (guard (exn (else
               (log/debug "Exception while examining file " (wrt path) ": "
                          (condition-message exn) " "
                          (wrt (condition-irritants exn)))
               #f))
    (with-exception-handler
      (lambda (exn)
        (if (and (warning? exn) (lexical-violation? exn)
                 (message-condition? exn) (irritants-condition? exn)
                 (source-condition? exn))
            (log/trace "Ignoring syntax error in " (wrt realpath) ": "
                       (condition-message exn) " with irritants "
                       (condition-irritants exn)
                       " at line " (source-line exn)
                       ", column " (source-column exn))
            (raise exn)))
      (lambda ()
        (call-with-input-file realpath
          (lambda (port)
            (let ((reader (make-reader port realpath)))
              (reader-tolerant?-set! reader #t)
              (let lp ((artifact* '()) (form-index 0) (datum (read-datum reader)))
                (let ((next-datum (read-datum reader)))
                  (cond ((and (not (eof-object? datum))
                              (or (maybe-program datum form-index next-datum reader)
                                  (maybe-library/module datum form-index next-datum)))
                         => (lambda (art*)
                              (if (or (r6rs-library? (car art*))
                                      (r7rs-library? (car art*)))
                                  (lp (append art* artifact*)
                                      (+ form-index 1)
                                      next-datum)
                                  (append art* artifact*))))
                        (else
                         (if (null? artifact*)
                             #f
                             artifact*))))))))))))

;; Examine files rejected by examine-source-file
(define (examine-other-file realpath path path-list)
    (define (maybe-legal)
      (and (regexp-matches rx-legal-notice-filename path)
           (list (make-legal-notice-file path path-list))))
    (maybe-legal)))
