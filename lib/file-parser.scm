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
    r6rs-program?
    r7rs-library? r7rs-library-name r7rs-library-exports
    r7rs-program?
    module?
    library-reference? library-reference-name library-reference-version-reference
    library-reference-original-import-spec
    library-reference-satisfied?
    include-reference? include-reference-path include-reference-realpath
    include-reference-conversion include-reference-original-include-spec)
  (import
    (rnrs (6))
    (only (srfi :1 lists) delete-duplicates)
    (srfi :115 regexp)
    (wak fmt)
    (xitomatl AS-match)
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
  (fields name version exports))

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
  (nongenerative))

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

;; Read all forms in an include-reference.
(define (include-reference-read-all include-ref)
  (call-with-input-file (include-reference-realpath include-ref)
    (lambda (port)
      (let lp ((datum* '()))
        (let ((datum (read port)))      ;TODO: use a compatible reader
          (if (eof-object? datum)
              (reverse datum*)
              (lp (cons datum datum*))))))))

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
                         (log/warn "Could not resolve include: " (list dir fn ext))
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
    (('include-file (((? str/sym? dir) ...) (? str/sym? fn)))
     (list (make-include-reference (make-path dir fn ".scm")
                                   (make-pkgpath dir fn ".scm")
                                   #f form)))
    (('include-file/downcase (((? str/sym? dir) ...) (? str/sym? fn)))
     (list (make-include-reference (make-path dir fn ".scm")
                                   (make-pkgpath dir fn ".scm")
                                   'downcase form)))
    ((? list? e*)
     (append-map (lambda (e)
                   (scan-for-includes/r6rs e realpath))
                 e*))
    (else '())))

;; Makes an include-reference for an R7RS-style include.
(define (mk-r7rs-include filename conversion original-include-spec from-lib-name from-realpath)
  (define (resolve-pathname path)       ;get rid of "." and ".."
    (fold-left path-join
               ""
               (let lp ((components (string-split path #\/)))
                 (match components
                   [("." . rest) (lp rest)]
                   [(".." . rest)
                    (assertion-violation 'mk-r7rs-include
                                         "Filename goes outside the repo" path)]
                   [(dir ".." . rest) (lp rest)]
                   [(x . y) (cons x (lp y))]
                   [() '()]))))
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
         (resolve-pathname (path-join (car (split-path from-realpath)) filename))))
    (make-include-reference
     target-path included-file-realpath
     conversion original-include-spec)))

;; Scan a list of R7RS <library declaration> forms. The filenames are
;; handled relative to the including file, although it is actually
;; implementation-specific and could be anything.
(define (parse-r7rs-library lib-name realpath decl*)
  (define visited-files (make-hashtable string-hash string=?))
  (define (parse-library-declaration decl realpath import* include* export*)
    (match decl
      (((or 'include 'include-ci) (? string? fn) (? string? fn*) ...)
       (log/debug "TODO: Scan R7RS include bodies for assets: " realpath)
       (let lp ((fn* (cons fn fn*)) (include* include*))
         (if (null? fn*)
             (values import* include* export*)
             (lp (cdr fn*)
                 (let* ((conversion (if (eq? (car decl) 'include-ci) 'foldcase #f))
                        (incl (mk-r7rs-include (car fn*) conversion decl lib-name realpath)))
                   (if (memp (lambda (x) (string=? (include-reference-path x)
                                                   (include-reference-path incl)))
                             include*)
                       include*         ;duplicate
                       (cons incl include*)))))))
      (('include-library-declarations (? string? fn) (? string? fn*) ...)
       (let ((asset* (map (lambda (fn) (mk-r7rs-include fn #f decl lib-name realpath))
                          (cons fn fn*))))
         (cond ((exists (lambda (asset)
                          (let ((fn (include-reference-realpath asset)))
                            (hashtable-ref visited-files fn #f)))
                        asset*)
                => (lambda (fn)
                     ;; XXX: This is a little arbitrary, and does not
                     ;; accept certain valid constructions, but this
                     ;; form is quite rarely used and this mustn't go
                     ;; into an infinite loop.
                     (assertion-violation 'parse-r7rs-library
                                          "File appears in include-library-declaration twice"
                                          fn))))
         (for-each (lambda (asset)
                     (let ((fn (include-reference-realpath asset)))
                       (hashtable-ref visited-files fn fn)))
                   asset*)
         (parse-decls (apply append (map include-reference-read-all asset*))
                      realpath import* (append asset* include*) export*)))
      (('export export-spec* ...)
       (let lp ((export-spec* export-spec*) (export* export*))
         (if (null? export-spec*)
             (values import* include* export*)
             (lp (cdr export-spec*)
                 (cons (match (car export-spec*)
                         [('rename from to) to]
                         [(? symbol? id) id])
                       export*)))))
      (('import import-spec* ...)
       (let lp ((import-spec* import-spec*) (import* import*))
         (if (null? import-spec*)
             (values import* include* export*)
             (lp (cdr import-spec*)
                 (cons (parse-r7rs-import-spec (car import-spec*))
                       import*)))))
      (('cond-expand (_feature-req* decl* ...) ...)
       (parse-decls (apply append decl*) realpath import* include* export*))
      (('begin body* ...)
       ;; The body can contain include, include-ci and load. In
       ;; practice this feature seems to be unused.
       (log/debug "TODO: Scan R7RS library bodies for assets: " realpath)
       (values import* include* export*))
      (x
       (log/debug "Unknown declaration in R7RS library: " (wrt x))
       (values import* include* export*))))

  (define (parse-decls decl* realpath import* include* export*)
    (let lp ((decl* decl*))
      (if (null? decl*)
          (values import* include* export*)
          (let-values (((import* include* export*) (lp (cdr decl*))))
            (parse-library-declaration (car decl*) realpath import* include* export*)))))

  (parse-decls decl* realpath '() '() '()))

(define parse-r7rs-import-spec
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
               ((exists r7rs-builtin-library? lib-name*)
                #t)
               (else #f)))))))

;; Examine a source file and return a list of artifact record, or #f
;; if it's probably not source code.
(define (examine-source-file realpath path path-list)
  (define (maybe-library/module form form-index next-datum)
    (match form
      (('library (name ...)             ;r6rs library
         ('export export* ...)
         ('import import* ...)
         . body*)
       (let ((ver (find list? name))
             (parsed-import-spec* (map parse-r6rs-import-spec import*)))
         (let ((include* (scan-for-includes/r6rs body* realpath)))
           (make-r6rs-library path path-list form-index (eof-object? next-datum)
                              parsed-import-spec* include*
                              (or (path->implementation-name path)
                                  (r6rs-library-name*->implementation-name
                                   (map library-reference-name parsed-import-spec*)))
                              (if ver (reverse (cdr (reverse name))) name)
                              (or ver '())
                              (append-map parse-r6rs-export export*)))))
      (('define-library (name ...)      ;r7rs library
         . declaration*)
       ;; XXX: name can contain <identifier> or <uinteger 10> [sic!].
       (let-values (((import* include* export*)
                     (parse-r7rs-library name realpath declaration*)))
         (make-r7rs-library path path-list form-index (eof-object? next-datum)
                            import* include* #f name export*)))
      (_                           ;some type of module or bare source
       (cond ((path->implementation-name path) =>
              (lambda (impl)
                (make-module path path-list form-index (eof-object? next-datum)
                             #f '() impl)))
             (else
              ;; TODO: detect modules from various implementations
              #f)))))
  (define (maybe-program form form-index next-datum port)
    (match form
      (('import import* ...)
       (cond
         ((r7rs-import-set? import*)
          (let ((parsed-import-spec* (map parse-r7rs-import-spec import*))
                (include* '()))
            (log/debug "TODO: Scan R7RS program bodies for assets: " realpath)
            (make-r7rs-program path path-list form-index #t parsed-import-spec* include*
                               #f)))
         (else
          ;; R6RS library
          (let ((parsed-import-spec* (map parse-r6rs-import-spec import*))
                (include* (let lp ((include* '()) (datum next-datum))
                            (if (eof-object? datum)
                                include*
                                (lp (append (scan-for-includes/r6rs datum realpath)
                                            include*)
                                    (read port))))))
            (make-r6rs-program path path-list form-index #t parsed-import-spec* include*
                               (or (path->implementation-name path)
                                   (r6rs-library-name*->implementation-name
                                    (map library-reference-name parsed-import-spec*))))))))
      (_ #f)))
  (log/trace "Examining source file: " (wrt realpath))
  (guard (exn (else
               (log/debug "Exception while examining file " (wrt path) ": "
                          (condition-message exn) " "
                          (wrt (condition-irritants exn)))
               #f))
    ;; FIXME: use a reader that handles non-r6rs code
    (call-with-input-file realpath
      (lambda (port)
        (let ((start (port-position port)))
          (let ((line1 (get-line port)))
            ;; Skip the shebang
            (unless (or (string-prefix? "#! " line1)
                        (string-prefix? "#!/" line1))
              (set-port-position! port start)))
          (let lp ((artifact* '()) (form-index 0) (datum (read port)))
            (let ((next-datum (read port)))
              (cond ((and (not (eof-object? datum))
                          (or (maybe-program datum form-index next-datum port)
                              (maybe-library/module datum form-index next-datum)))
                     => (lambda (artifact)
                          (if (r6rs-library? artifact)
                              (lp (cons artifact artifact*)
                                  (+ form-index 1)
                                  next-datum)
                              (cons artifact artifact*))))
                    (else
                     (if (null? artifact*)
                         #f
                         artifact*))))))))))

;; Examine files rejected by examine-source-file
(define (examine-other-file realpath path path-list)
    (define (maybe-legal)
      (and (regexp-matches rx-legal-notice-filename path)
           (list (make-legal-notice-file path path-list))))
    (maybe-legal)))
