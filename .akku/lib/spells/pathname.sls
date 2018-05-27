#!r6rs #!r6rs
;;; pathname.sls --- Portable Pathname Abstraction

;; Copyright (C) 2009, 2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Ported to Scheme and modified by Andreas Rottmann.
;;
;; Original ELisp code written by Taylor Campbell and placed in the
;; Public Domain.

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

;;@ Pathname abstraction.
(library (spells pathname)
  (export make-file
          file?
          file-name
          file-type
          file-types
          file-version
          ->file

          make-pathname
          pathname?
          ->pathname
          pathname=?
          pathname-compare
          pathname<?
          pathname>?
          pathname-hash
          
          pathname-origin
          pathname-directory
          pathname-file
          pathname-components
          pathname-with-origin
          pathname-with-directory
          pathname-with-file
          pathname-default
          merge-pathnames
          enough-pathname
          directory-pathname?
          pathname-as-directory
          pathname-container
          pathname-join
          
          directory-namestring
          file-namestring
          origin-namestring
          ->namestring

          parse-unix-file-types
          local-file-system-type

          ;; Deprecated R5RS-compatible aliases
          (rename (->file x->file)
                  (->pathname x->pathname)
                  (->namestring x->namestring)))
  (import (rnrs base)
          (only (rnrs hashtables) string-hash symbol-hash)
          (except (srfi :1 lists) map for-each)
          (srfi :8 receive)
          (srfi :9 records)
          (except (srfi :13 strings)
                  string-hash
                  string-copy
                  string->list
                  string-for-each)
          (srfi :14 char-sets)
          (srfi :39 parameters)
          (spells hash-utils)
          (spells record-types)
          (spells opt-args)
          (spells operations)
          (spells match)
          (spells string-utils)
          (spells pathname os-string))

;;@extractors (import (spells private stexidoc)) spells-extractors

;;@subheading Introduction
;;
;; @emph{Pathnames} are platform-independent representations of
;; locations of objects on file systems.  A pathname consists of three
;; components:
;;
;; @itemize @bullet
;; @item
;;      The @emph{origin} is the place from which the path begins.  It
;;      might be the Unix root directory, a user's home directory, a
;;      DOS device/drive, an Apollo logical name, a Unix environment
;;      variable, a VMS host, &c.  A pathname with @code{#f} as
;;      origin is a relative pathname.
;; @item
;;      The @emph{directory} is a list of directory names from the
;;      origin leading up to the file.  Each directory name is a string.
;; @item
;;      The @emph{filename} is the name of the file itself along with
;;      the type and version.  The name is a string.  There may be
;;      zero or more types, each of which is also a string.  The
;;      version is either a non-negative integer or @code{#f},
;;      implying the newest version.
;; @end itemize

;;@stop

;;++ Still missing:
;;++
;;++   - non-Unix file system types
;;++   - pathname expansion
;;++   - truename operation

;;;; Utilities

(define (substring-split s sep start)
  (if (= 0 start)
      (string-split s sep)
      (string-split (substring/shared s start (string-length s)) sep)))


;;;; Pathnames

;;;@subheading Pathname construction

;;@ Pathname disjoint type.
(define-record-type* pathname
  (really-make-pathname origin directory file)
  ())

;;@ Construct a pathname.
;;
;; A pathname with @1 and @2 as the origin and directory fields is
;; returned. The file field is derived from @3, which may be either a
;; file object, a string (representing the file name), a list of one
;; or more strings (representing the file name and types), or
;; @code{#f}, representing an empty file field.
(define (make-pathname origin directory file)
  (define (lose message . irritants)
    (apply assertion-violation 'make-pathname message irritants))
  (really-make-pathname
   origin
   (%->strlist directory (lambda () (lose "invalid directory part" directory)))
   (cond ((list? file)
          (case (length file)
            ((0) (lose "empty list not allowed for file part"))
            ((1) (make-file (car file) #f))
            (else (make-file (car file) (cdr file)))))
         ((or (string? file)
              (symbol? file))
          (make-file file #f))
         ((or (file? file)
              (eqv? #f file))
          file)
         (else
          (lose "invalid argument type for file part" file)))))

;;@ Coerce @1 to a pathname.
;;
;; If @1 is a symbol, return a pathname with a relative origin, an
;; empty directory, and a file whose name is the symbol.  If @1 is a
;; string, parse it according to the optional file system type @2,
;; which defaults to the local file system type.  If @1 is a pathname,
;; return it.  In any other case, signal an error.
;;
;; @emph{FIXME}: Document how pairs are parsed.
(define* (->pathname object (fs-type (local-file-system-type)))
  (define (lose)
    (assertion-violation '->pathname "cannot coerce to a pathname" object))
  (cond ((symbol?    object) (make-pathname #f '() object))
        ((string?    object) (parse-namestring object fs-type))
        ((os-string? object) (parse-namestring (os-string->string object)))
        ((pathname?  object) object)
        ((pair? object)
         (match object
           (((dir ___))
            (make-pathname #f dir #f))
           (((dir ___) file)
            (make-pathname #f dir file))
           ((origin (dir ___) file)
            (make-pathname origin dir file))
           (else
            (lose))))
        (else
         (lose))))

;;@ Coerce @1 to a file.
;;
;; Parse @1, which may be a string or a symbol, as file component,
;; according to the optional file system type @2, which defaults to
;; the local file system type.
(define* (->file object (fs-type (local-file-system-type)))
  (fs-type/parse-file-namestring
   fs-type
   (%->string object
              (lambda ()
                (assertion-violation '->file "cannot coerce to file" object)))))


;;;; Files

(define-record-type file
  (really-make-file name types version)
  file?
  (name file-name)
  (types file-types)
  (version file-version))

;;@ Make a pathname file with the given components. @1 is the file's
;; name, a string or a symbol.  @var{type} is the file's type or
;; types; it may be a string, a symbol, or a list of strings and
;; symbols. @var{version} is a non-negative integer representing the
;; file's version, or the symbol @code{newest} representing the most
;; recent version of the file.
(define* (make-file name type (version #f))
  (define (lose)
    (assertion-violation 'make-file "Invalid file name specifier" name))
  (really-make-file (%->string name lose) (make-file/type type) version))

;;@stop

(define (%->strlist lst lose)
  (map (lambda (item)
         (%->string item lose))
       lst))

(define (%->string thing lose)
  (cond ((string? thing) thing)
        ((symbol? thing) (symbol->string thing))
        (else           (lose))))

(define (make-file/type type)
  (define (lose)
    (assertion-violation 'make-file "Invalid file type specifier" type))
  (cond ((not type)     '())
        ((string? type)  (list type))
        ((symbol? type)  (list (symbol->string type)))
        ((list? type)    (%->strlist type lose))
        (else            (lose))))

;;@ Return the type of @1.
;; Return the last type if there is more than one.
;; Return @code{#f} if the file has no type.
(define (file-type file)
  (cond ((null? (file-types file)) #f)
        (else                      (last (file-types file)))))

;;@ Return @code{#t} if @1 is a valid file version and code{#f} if not.
(define (file-version? object)
  (or (not object)                     ; No version.
      (and (integer? object) (>= object 0))))

;;;@subheading Pathname Component Substitution & Merging

;;@ Return a pathname like @1 with an origin of @2.
(define (pathname-with-origin pathname origin)
  (let ((pathname (->pathname pathname)))
    (make-pathname origin
                   (pathname-directory pathname)
                   (pathname-file pathname))))

;;@ Return a pathname like @1 with a directory of @2.
(define (pathname-with-directory pathname directory)
  (let ((pathname (->pathname pathname)))
    (make-pathname (pathname-origin pathname)
                   directory
                   (pathname-file pathname))))

;;@ Return a pathname like @1 with a file of @2.
(define (pathname-with-file pathname file)
  (let ((pathname (->pathname pathname)))
    (make-pathname (pathname-origin pathname)
                   (pathname-directory pathname)
                   file)))

;;@ Return the origin, directory and file components of @1.
(define (pathname-components pathname)
  (values (pathname-origin pathname)
          (pathname-directory pathname)
          (pathname-file pathname)))

;;@ Return a pathname like @1.
;; Any null components of @1 are filled with the supplied
;; arguments, @1, @2 and @3.
(define (pathname-default pathname origin directory file)
  (let ((pathname (->pathname pathname)))
    (make-pathname (or (pathname-origin pathname) origin)
                   (or (pathname-directory pathname) directory)
                   (or (pathname-file pathname) file))))

;;@ Return a pathname by merging @1 with @2.
(define (merge-pathnames pathname defaults-pathname)
  (let* ((pathname (->pathname pathname))
         (defaults-pathname (->pathname defaults-pathname))
         (origin (pathname-origin pathname))
         (origin-default (pathname-origin defaults-pathname))
         (directory (pathname-directory pathname))
         (directory-default (pathname-directory defaults-pathname))
         (file (merge-files (pathname-file pathname)
                            (pathname-file defaults-pathname))))
    (cond ((not origin)
           (make-pathname origin-default
                          (cond ((null? directory) directory-default)
                                ((null? directory-default) directory)
                                (else (append directory-default directory)))
                          file))
          ((origin-back-count origin)
           => (lambda (n-backs)
                (let* ((n-drop (min n-backs (length directory-default)))
                       (n-left (- n-backs n-drop)))
                  (make-pathname (or origin-default
                                     (and (> n-left 0)
                                          (make-list n-left 'back)))
                                 (append (drop-right directory-default n-drop)
                                         directory)
                                 file))))
          (else
           (make-pathname origin directory file)))))

;;@stop

(define (origin-back-count origin)
  (if (not origin)
      0
      (let loop ((origin origin) (n 0))
        (cond ((null? origin)
               n)
              ((and (pair? origin) (eq? 'back (car origin)))
               (loop (cdr origin) (+ n 1)))
              (else
               #f)))))

;;@ Return a file by merging @1 with @2.
(define (merge-files file defaults-file)
  (cond ((not file)          defaults-file)
        ((not defaults-file) file)
        (else
         (let ((name (file-name file))
               (defaults-name (file-name defaults-file))
               (types (file-types file))
               (defaults-types (file-types defaults-file))
               (version (file-version file))
               (defaults-version (file-version defaults-file)))
           (make-file (or name defaults-name)
                      (if (null? types) defaults-types types)
                      (or version defaults-version))))))

;;@ Return a pathname whose merging with @var{relative} produces
;; @var{pathname}.
(define (enough-pathname pathname relative)
  (define (lose)
    (error 'enough-pathname
           "cannot represent pathname relatively" pathname relative))
  (let* ((pathname (->pathname pathname))
         (relative (->pathname relative))
         (origin (pathname-origin pathname))
         (relative-origin (pathname-origin relative)))
    (cond ((equal? origin relative-origin)
           (receive (relative-directory directory)
                    (drop-prefix string=?
                                 (pathname-directory relative)
                                 (pathname-directory pathname))
             (cond ((null? relative-directory)
                    (make-pathname #f directory (pathname-file pathname)))
                   (else
                    (make-pathname (make-list (length relative-directory)
                                              'back)
                                   directory
                                   (pathname-file pathname))))))
          (else
           (let ((n-backs (origin-back-count origin))
                 (relative-n-backs (origin-back-count relative-origin)))
             (cond ((and n-backs relative-n-backs)
                    (if (< n-backs relative-n-backs)
                        (lose)
                        (make-pathname
                         (make-list (+ (length (pathname-directory relative))
                                       (- n-backs relative-n-backs))
                                    'back)
                         (pathname-directory pathname)
                         (pathname-file pathname))))
                   ((not n-backs)
                    ;; `pathname' is absolute
                    pathname)
                   (else
                    ;; `pathname' is relative, but `relative' is not
                    (lose))))))))

;;@stop

(define (drop-prefix =? xs ys)
  (let loop ((xs xs) (ys ys))
    (cond ((or (null? xs) (null? ys))
           (values xs ys))
          ((=? (car xs) (car ys))
           (loop (cdr xs) (cdr ys)))
          (else
           (values xs ys)))))


;;;@subheading Directory Pathnames

;;@ Returns @code{#t} if @var{pathname} has a directory component, but
;; no file component, and @code{#f} if otherwise.
(define (directory-pathname? pathname)
  (let ((pathname (->pathname pathname)))
    (and (pathname-directory pathname)
         (not (pathname-file pathname)))))

;;@ Return a pathname like @1, representing a directory.
;; If @1 has a file component, it is added to the end of the list of
;; directory components, and the resultant pathname has no file.
;; Otherwise, return @1.
(define (pathname-as-directory pathname)
  (let* ((pathname (->pathname pathname))
         (file (pathname-file pathname)))
    (if file
        (make-pathname (pathname-origin pathname)
                       (let ((directory (pathname-directory pathname)))
                         (if (null? directory)
                             (list (file-namestring pathname))
                             (append directory (list (file-namestring pathname)))))
                       #f)
        pathname)))

;;@ Return a pathname of the directory that contains @1.
(define (pathname-container pathname)
  (let loop ((pathname (->pathname pathname)))
    (let ((origin (pathname-origin pathname))
          (directory (pathname-directory pathname))
          (file (pathname-file pathname)))
      (cond (file
             (make-pathname origin directory #f))
            ((and directory (not (null? directory)))
             (make-pathname origin (drop-right directory 1) #f))
            (else
             (let ((expansion (expand-pathname pathname)))
               (if (pathname=? expansion pathname)
                   (error 'pathname-container
                          "Unable to find pathname's container"
                          pathname)
                   (loop expansion))))))))

;;@ Return a pathname by interpreting @2 as a series of directories
;; relative to the origin and directory of @1, with the last element
;; of @2 specifying the file component of the result.
(define (pathname-join base . steps)
  (fold (lambda (step pathname)
          (merge-pathnames step pathname))
        base
        steps))

;;;@subheading Hashing and comparison

(define null-hash 0)

(define (file-hash file)
  (if file
      (hash-fold string-hash
                 (string-hash (file-name file))
                 (file-types file))
      null-hash))

(define (pathname-hash pathname)
  (receive (origin dir file) (pathname-components pathname)
    (hash-combine (cond ((pair? origin)  (hash-fold symbol-hash null-hash origin))
                        ((symbol? origin) (symbol-hash origin))
                        ((string? origin) (string-hash origin))
                        (else             null-hash))
                  (hash-fold string-hash (file-hash file) dir))))

;;@ Compare the pathnames @1 and @2 and return @code{#t} if they refer
;; to the same filesystem entity.
(define (pathname=? x y)
  (and (equal? (pathname-origin x) (pathname-origin y))
       (= (pathname-compare x y) 0)))

;;@stop

(define (string-cmp x y)
  (string-compare x y (lambda (i) -1) (lambda (i) 0) (lambda (i) 1)))

(define (strlist-compare x y)
  (let loop ((x x) (y y))
    (cond ((and (null? x) (null? y))
           0)
          ((null? x)
           -1)
          ((null? y)
           1)
          (else
           (let ((elt-cmp (string-cmp (car x) (car y))))
             (if (= 0 elt-cmp)
                 (loop (cdr x) (cdr y))
                 elt-cmp))))))

(define (version-compare x y)
  (if (and (eqv? x #f) (eqv? y #f))
      0
      (let ((delta (- x y)))
        (cond ((< delta 0) -1)
              ((= delta 0)  0)
              (else         1)))))

(define (file-compare x y)
  (cond ((and (eqv? x #f) (eqv? y #f))
         0)
        ((eqv? x #f)
         -1)
        ((eqv? y #f)
         1)
        (else
         (let ((name-cmp (string-cmp (file-name x) (file-name y))))
           (if (= 0 name-cmp)
               (let ((types-cmp (strlist-compare (file-types x)
                                                 (file-types y))))
                 (if (= 0 types-cmp)
                     (version-compare (file-version x) (file-version y))
                     types-cmp))
               name-cmp)))))

;;@ Compare the pathnames @1 and @2, without considering the
;; origin. Returns 0 on equality, -1 when @1 is considered less than
;; @2, and 1 if it is considered greater.
(define (pathname-compare x y)
  (let ((dir-cmp (strlist-compare (pathname-directory x)
                                  (pathname-directory y))))
    (if (= dir-cmp 0)
        (file-compare (pathname-file x) (pathname-file y))
        dir-cmp)))

;;@ Boolean predicates based on @ref{pathname-compare}.
(define (pathname<? x y)
  (< (pathname-compare x y) 0))

(define (pathname>? x y)
  (> (pathname-compare x y) 0))


;;;; Pathname Expansion

;; Return a pathname like @1 but with the origin expanded.
;; This is currently unimplemented and will simply return @1.
(define (expand-pathname pathname)
  ;;(error 'expand-pathname "Unimplemented: %S" `(expand-pathname ',pathname))
  pathname)


;;;@subheading Namestrings

;;@ Parse @1 and return a pathname representing it.
;; Use @2's namestring parser to parse @1.
;; If @2 is not supplied, it defaults to the local file system."
(define* (parse-namestring namestring (fs-type (local-file-system-type)))
  (fs-type/parse-namestring fs-type namestring))

;;@ Coerce @1 into a namestring.
;; If @1 is a string, canonicalize it according to @2.
;; If @1 is a pathname, convert it according to @2.
;; Otherwise, raise an @code{&assertion} condition.
;; If @2 is not supplied, it defaults to the local file system type."
(define* (->namestring object (fs-type (local-file-system-type)))
  ;++ What if it's a symbol?  Use (MAKE-PATHNAME NIL NIL object)?
  (cond ((string? object)
         (fs-type/canonicalize-namestring fs-type object))
        ((pathname? object)
         (pathname->namestring object fs-type))
        ((pair? object)
         (pathname->namestring (->pathname object) fs-type))
        (else
         (assertion-violation '->namestring
                              "Unable to coerce to a namestring" object))))

(define (pathname->namestring pathname fs-type)
  (fs-type/pathname->namestring fs-type pathname))

;;@ Return a string for @1's origin according to @2.
;; If @2 is not supplied, it defaults to the local file system type.
(define* (origin-namestring pathname (fs-type (local-file-system-type)))
  (fs-type/origin-namestring fs-type (->pathname pathname)))

;;@ Return a string for @1's directory according to @2.
;; If @2 is not supplied, it defaults to the local file system type.
(define* (directory-namestring pathname (fs-type (local-file-system-type)))
  (fs-type/directory-namestring fs-type (->pathname pathname)))

;;@ Return a string for @1's file according to @2.
;; If @2 is not supplied, it defaults to the local file system type.
(define* (file-namestring pathname (fs-type (local-file-system-type)))
  (fs-type/file-namestring fs-type (->pathname pathname)))

;;@ Return a string naming @var{pathname} relative to @var{relative},
;; according to @var{fs-type}.  If @var{fs-type} is not supplied, it
;; defaults to the local file system type."
(define* (enough-namestring pathname
                            relative
                            (fs-type (local-file-system-type)))
  (fs-type/pathname->namestring (enough-pathname pathname relative) fs-type))


;;;@subheading File System Types

(define-operation (fs-type/parse-namestring fs-type namestring))

(define-operation (fs-type/canonicalize-namestring fs-type object)
  (pathname->namestring (->pathname object) fs-type))

(define-operation (fs-type/origin-namestring fs-type pathname))
(define-operation (fs-type/directory-namestring fs-type pathname))
(define-operation (fs-type/file-namestring fs-type pathname))

(define-operation (fs-type/pathname->namestring fs-type pathname)
  (string-append (origin-namestring pathname)
                 (directory-namestring pathname)
                 (file-namestring pathname)))

(define-operation (fs-type/parse-file-namestring fs-type namestring)
  (make-file namestring '() #f))

(define unix-file-system-type
  (object #f
    ((fs-type/origin-namestring self pathname)
     (let ((origin (pathname-origin pathname)))
       (define (lose)
         (error 'unix/origin-namestring
                "invalid origin for unix file system" origin))
       (cond ((or (not origin) (null? origin)) "")
             ((or (eq? origin '/)) "/")
             ((pair? origin)
              (let loop ((o origin) (parts '()))
                (if (null? o)
                    (string-join (reverse parts) "/" 'suffix)
                    (case (car o)
                      ((back) (loop (cdr o) (cons ".." parts)))
                      (else
                       (lose))))))
             (else
              (lose)))))
    
    ((fs-type/directory-namestring self pathname)
     (let ((dir (pathname-directory pathname)))
       (if (null? dir)
           "."
           (string-append (string-join dir "/") "/"))))

    ((fs-type/pathname->namestring self pathname)
     
     (string-append (fs-type/origin-namestring self pathname)
                    (cond ((and (null? (pathname-directory pathname))
                                (not (pathname-file pathname)))
                           
                           ".")
                          ((null? (pathname-directory pathname))
                           "")
                          (else
                           (fs-type/directory-namestring self pathname)))
                    (fs-type/file-namestring self pathname)))
    
    ((fs-type/file-namestring self pathname)
     (let ((file (pathname-file pathname)))
       (if (not file)
           ""
           (string-concatenate
            (cons (file-name file)
                  (if (null? (file-types file))
                      '()
                      (list "." (string-join (file-types file) "."))))))))
    
    ((fs-type/parse-namestring self namestring)
     (let ((parts (remove string-null? (string-split namestring #\/)))
           (absolute? (string-prefix? "/" namestring))
           (directory? (string-suffix? "/" namestring)))
       (receive (origin parts directory?)
                (normalize-directory parts directory?)
         (make-pathname
          (if absolute? '/ origin)
          (if directory? parts (drop-right parts 1))
          (if directory?
              #f
              (let ((file-part (last parts)))
                (fs-type/parse-file-namestring self file-part)))))))

    ((fs-type/parse-file-namestring self namestring)
     (if (parse-unix-file-types)
         (receive (prefix file-parts)
                  (cond ((string-every #\. namestring)
                         (values "" (list namestring)))
                        ((string-skip namestring #\.)
                         => (lambda (idx)
                              (values (substring/shared namestring 0 idx)
                                      (substring-split namestring #\. idx))))
                        (else (values "" (string-split namestring #\.))))
           (if (and (null? file-parts) (string-null? prefix))
               #f
               (make-file (string-append prefix (first file-parts))
                          (cdr file-parts))))
         (make-file namestring '())))))

(define (normalize-directory parts directory?)
  (let loop ((parts parts) (n-backs 0) (dir '()) (dir? #f))
    (if (null? parts)
        (values (if (= n-backs 0)
                    #f
                    (make-list n-backs 'back))
                (reverse dir)
                (or directory? dir?))
        (let ((part (car parts)))
          (cond ((string=? part ".")
                 (loop (cdr parts) n-backs dir #t))
                ((string=? part "..")
                 (if (null? dir)
                     (loop (cdr parts) (+ n-backs 1) dir #t)
                     (loop (cdr parts) n-backs (cdr dir) #t)))
                (else
                 (loop (cdr parts) n-backs (cons part dir) #f)))))))

;;@defun parse-unix-file-types
;;@defunx parse-unix-file-types boolean
;;
;; An @uref{http://srfi.schemers.org/srfi-39/srfi-39.html,SRFI 39}
;; parameter controlling if file types are parsed by the UNIX file
;; system type.
;;
;;@end defun
(define parse-unix-file-types (make-parameter #t))

;;@defun local-file-system-type
;;
;; Returns the local file system type.
;;
;;@end defun
(define local-file-system-type (make-parameter unix-file-system-type))

;; these go last, since it may expand to an expression, not a definition
(define-record-discloser pathname
  (lambda (pathname)
    (list 'pathname
          (pathname-origin pathname)
          (pathname-directory pathname)
          (pathname-file pathname))))

(define-record-discloser file
  (lambda (file)
    (list 'file (file-name file) (file-types file) (file-version file))))

)
