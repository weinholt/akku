#!r6rs #!r6rs
;;; filesys.scm --- Filesystem interface.

;; Copyright (C) 2009-2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;; This file contains the non-primitive procedures that can be defined
;; in terms of primitives.

;;; Code:

;;@ File system interface.
;;
;; This library accepts pathnames, for which @pxref{(spells
;; pathname)}.
(library (spells filesys)
  (export file-exists?
          create-directory
          create-directory*
          delete-file
          rename-file
          copy-file
          install-file
          create-symbolic-link
          create-hard-link
          create-temp-file
          create-temp-directory
          temp-pathname-iterate
          
          file-regular?
          file-directory?
          file-symbolic-link?
          file-readable?
          file-writable?
          file-executable?
          file-modification-time
          file-size-in-bytes

          directory-stream?
          open-directory-stream
          close-directory-stream
          read-directory-stream
          in-directory-stream
          in-directory
          
          directory-fold*
          directory-fold
          directory-fold-tree*
          directory-fold-tree

          file-unreachable-error?
          file-unreachable-error-pathname
          file-unreachable-error-operator

          working-directory
          with-working-directory

          call-with-input-file-and-directory
          call-with-output-file/atomic

          find-file
          library-search-paths)
  (import (except (rnrs) delete-file file-exists?)
          (only (srfi :1 lists) fold)
          (srfi :8 receive)
          (srfi :19 time)
          (only (srfi :13)
                string-concatenate
                string-contains
                string-unfold)
          (srfi :27)
          (srfi :98 os-environment-variables)
          (only (spells process) get-process-id)
          (spells string-utils)
          (spells pathname)
          (spells ports)
          (spells filesys compat))


;;; @subsection Individual file operations

;;@defun file-exists? pathname
;;
;; Returns true if a file or directory exists on the file system at a
;; location specified by @var{pathname}.
;;
;;@end defun

;;@defun create-directory pathname
;;
;; Creates a new directory at @var{pathname}. It is an error if there
;; already exists an object at the location designated by
;; @var{pathname}.
;;
;;@end defun

;;@defun delete-file pathname
;;
;; Deletes the object on the file system at the location specified by
;; @var{pathname} (be it a regular file, a directory, or anything
;; else).  If there is no such object, nothing is done.  It is an
;; error if @var{pathname} designates a directory that is not empty or
;; the operating system does not permit its deletion by the Scheme
;; program.
;;
;;@end defun

;;@defun rename-file old-pathname new-pathname
;;
;; Renames or moves the file specified by @var{old-pathname} to
;; @var{new-pathname}. @var{new-pathname} may be a file pathname, in
;; which case the file is given that exact name; it may also be a
;; directory pathname, in which case the file is moved into that
;; directory.  The object on the file system specified by
;; @var{old-pathname} may be any kind of file: regular, directory, &c.
;; It is an error if the Scheme program is not permitted by the
;; operating system to perform this operation, @var{old-pathname} does
;; not exist, or @var{new-pathname} designates a file that is not in a
;; directory that already exists and in which files are allowed by the
;; operating system to be created by the Scheme program.
;;
;;@end defun

;;@defun file-regular? pathname
;;@defunx file-symbolic-link? pathname
;;@defunx file-directory? pathname
;;@defunx file-readable? pathname
;;@defunx file-writable? pathname
;;
;; These all test various attributes of the file specified by
;; @var{pathname}.  @code{file-regular?} tests whether the file is a
;; regular file, rather than a directory or other kind of object on
;; the file system; @code{file-directory?}  tests whether the file is
;; a directory; @code{file-readable?} tests whether the file can be
;; read by the Scheme program, i.e. opening an input port over it will
;; not fail; and @code{file-writable?} tests whether a file specified
;; by @var{pathname} could be written to, i.e. opening an output port
;; to it would not fail.
;;
;; @code{file-regular?} and @code{file-directory?} return false if
;; there is no object on the file system designated by @var{pathname}.
;;
;;@end defun

;;@defun file-modification-time pathname
;;
;; Returns a SRFI 19 time object that represents the last time that
;; the file on the file system at the location specified by
;; @var{pathname} was modified or the time that it was created.  It is
;; an error if the operating system does not allow the Scheme program
;; to access this attribute of the file or the file does not already
;; exist.
;;
;;@end defun

;;@defun file-size-in-bytes pathname
;;
;; Returns the number of bytes that the regular file designated by
;; @var{pathname} contains.  The effect is unspecified if
;; @var{pathname} is not a regular file.  It is an error if the file
;; does not already exist or the Scheme program is not permitted by
;; the operating system to probe this attribute of the file.
;;
;;@end defun

;;;@subsection Directory content operations
;;
;; The operations in this section deal with directory contents.

;;;@subsubheading Primitives
;;
;; These are the procedures that the rest of directory content
;; operations are built upon; you might want to consider using the
;; foof-loop operators instead, which provide a more convenient
;; interface. 

;;@defun directory-stream? object
;; Return @code{#t} if @var{object} is a directory stream, @code{#f}
;; otherwise.
;;@end defun

;;@defun open-directory-stream pathname
;;
;; Open the directory specified by @var{pathname} for reading its
;; contents.  Returns a directory stream.
;;
;;@end defun

;;@defun close-directory-stream stream
;;
;; Close the directory stream @var{stream}, freeing associated
;; resources.
;;
;;@end defun

;;@defun read-directory-stream stream
;;
;; Read an entry from @var{stream}.
;;
;;@end defun

;;@subsubheading Foof-loop integration
;;
;; Foof-loop is an extensible loop facility that allows for concise
;; and convenient expression of complex loops. The iterators described
;; in the following extend foof-loop to allow for iteration over
;; directory contents.  @xref{Top,,the foof-loop documentation,(wak
;; foof-loop)}, for more information about how to use foof-loop.

;;@
;; @lisp
;; (loop (for entry (in-directory-stream @var{stream}))
;;   ...)@end lisp
;;
;; Loop over the entries in @var{stream}, a directory stream.
(define-syntax in-directory-stream
  (syntax-rules ()
    ((_ (elt-var) (stream-expr) cont . env)
     (cont
      (((stream) stream-expr))         ;Outer bindings
      ()                               ;Loop variables
      (((elt-var) (read-directory-stream stream))) ;Entry bindings
      ((not elt-var))                  ;Termination conditions
      ()                               ;Body bindings
      ()                               ;Final bindings
      . env))))

;;@
;; @lisp
;; (loop (for entry (in-directory-stream @var{stream}))
;;   ...)@end lisp
;;
;; Loop over the directory entries of @var{pathname}, which must refer
;; to a directory.
(define-syntax in-directory
  (syntax-rules ()
    ((_ (elt-var) (pathname-expr) cont . env)
     (cont
      (((stream)                       ;Outer bindings
        (open-directory-stream pathname-expr)))
      ()                               ;Loop variables
      (((elt-var)                      ;Entry bindings
        (read-directory-stream stream)))
      ((not elt-var))                  ;Termination conditions
      ()                               ;Body bindings
      ((()                            ;Final bindings
        (begin (close-directory-stream stream)
               (values))))
      . env))))

;;@subsubheading Directory folding
;;
;; @quotation Note
;;
;; The procedures in this section are tentatively deprecated; new code
;; should use foof-loop in combination with the above iterators, as
;; this provides more flexibility with a more intuitive interface.
;;
;; @end quotation

;;@
;;
;; Folds every file in the directory specified by @var{pathname} by
;; @var{combiner}.  That is, for every file in the directory,
;; @var{combiner} is passed the full pathname & the current set of
;; state seeds.  @var{combiner} should return @math{N+1} values, where
;; @math{N} is the number of seeds: a boolean that, if true, specifies
;; that the iteration should continue, or, if false, specifies that
;; the iteration should halt; and the next set of state seeds.  When
;; the iteration is halted, either because @var{combiner} returned
;; @code{#f} as its first value or because there are no more files in
;; the directory, the current set of state seeds is returned.  There
;; is no reliable ordering of the filenames passed to @var{combiner}.
;;
;; It is an error if there is no object specified by @var{pathname},
;; the object on the file system is not a directory, or the operating
;; system does not permit the Scheme program to read the directory's
;; contents.
;;
;; Examples:
;;
;; @lisp
;;   ;; Return a list of the full pathnames of all files in a directory.
;;   (directory-fold* directory
;;     (lambda (pathname list)
;;       (values #t (cons pathname list)))
;;     '())
;;
;;   ;; Compute a list of the full pathnames of all subdirectories of a
;;   ;; directory.
;;   (directory-fold* directory
;;     (lambda (pathname list)
;;       (values #t (if (file-directory? pathname)
;;                      (cons pathname list)
;;                      list)))
;;     '())
;;
;;   ;; Find the (shallow) sum of the number of bytes in all files in a
;;   ;; directory.
;;   (directory-fold* directory
;;     (lambda (pathname sum)
;;       (values #t (if (file-regular? pathname)
;;                      (+ sum (file-size-in-bytes pathname))
;;                      sum)))
;;     0)
;;
;;   ;; Return the full pathname of the first file in a directory that
;;   ;; satisfies some predicate, or #F if no such file exists.
;;   (directory-fold* directory
;;     (lambda (pathname false)
;;       (if (predicate? pathname)
;;           (values #f pathname)
;;           (values #t #f)))
;;     #f)
;; @end lisp
(define (directory-fold* pathname combiner . seeds)
  (let* ((dirname (pathname-as-directory pathname))
         (stream (open-directory-stream dirname)))
    (define (full-pathname entry)
      (pathname-with-file dirname (pathname-file (->pathname entry))))
    (let loop ((seeds seeds))
      (let ((entry (read-directory-stream stream)))
        (if (not entry)
            (apply values seeds)
            (receive (continue? . new-seeds)
                     (apply combiner (full-pathname entry) seeds)
              (if continue?
                  (loop new-seeds)
                  (apply values new-seeds))))))))

;;@
;;
;; Simplified variant of @code{directory-fold*} that does not support
;; premature termination.  This is equivalent to:
;;
;; @lisp
;;  (define (directory-fold pathname combiner . seeds)
;;    (apply
;;      directory-fold* pathname
;;      (lambda (dir-entry . seeds)
;;        (receive new-seeds (apply combiner dir-entry seeds)
;;          (apply values #t new-seeds)))
;;      seeds)
;; @end lisp
;;
;; Some of the above examples are simplified by DIRECTORY-FOLD; e.g.,
;; to produce a list of the full pathnames of all files in a
;; directory, one can use:
;;
;; @lisp
;;   (directory-fold directory cons '())
;; @end lisp
(define (directory-fold pathname combiner . seeds)
  (apply
   directory-fold* pathname
   (lambda (dir-entry . seeds)
     (receive new-seeds (apply combiner dir-entry seeds)
       (apply values #t new-seeds)))
   seeds))


;;@
;;
;; This is like @code{directory-fold}, but it walks down entire trees
;; of directories.  For each entry in the directory named by @1: if
;; that entry is a non-directory, @var{combiner} is passed the full pathname of
;; that file and the current seeds, and the walk of the directory's
;; entries proceeds with the new seeds; if that entry is a directory,
;; @3 is passed the full pathname of that directory & the current
;; seeds, and the seeds it returns are used to recursively descend
;; into the directory.  When the recursive descent returns, the seeds
;; it returned are used to proceed the walk of the enclosing
;; directory's entries.  This could be defined as follows:
;;
;; @lisp
;;   (define (directory-fold-tree pathname file-combiner dir-combiner
;;                                . seeds)
;;     (apply directory-fold pathname
;;            (lambda (pathname . seeds)
;;              (if (file-directory? pathanme)
;;                  (receive new-seeds
;;                           (apply dir-combiner pathname seeds)
;;                    (apply directory-fold-tree pathname
;;                           file-combiner dir-combiner
;;                           new-seeds))
;;                (apply file-combiner pathname seeds)))
;;            seeds))
;; @end lisp
;;
;; However, it is likely to be implemented much more efficiently with
;; respect to the underlying file system.
(define (directory-fold-tree* pathname file-combiner dir-combiner . seeds)
  (apply directory-fold* pathname
         (lambda (pathname . seeds)
           (if (file-directory? pathname)
               (receive (new-fc new-dc proceed . new-seeds)
                        (apply dir-combiner pathname seeds)
                 (cond ((and new-fc new-dc)
                        (receive newest-seeds
                                 (apply directory-fold-tree*
                                        (pathname-as-directory pathname)
                                        (if (eqv? new-fc #t) file-combiner new-fc)
                                        (if (eqv? new-dc #t) dir-combiner new-dc)
                                        new-seeds)
                          (if proceed
                              (apply proceed newest-seeds)
                              (apply values #t newest-seeds))))
                       ((or new-fc new-dc)
                        (apply values #t new-seeds))
                       (else
                        (apply values #f new-seeds))))
               (apply file-combiner pathname seeds)))
         seeds))

(define (directory-fold-tree pathname file-combiner dir-combiner . seeds)
  (apply directory-fold-tree* pathname
         (lambda (file-entry . seeds)
           (receive new-seeds (apply file-combiner file-entry seeds)
             (apply values #t new-seeds)))
         (lambda (dir-entry . seeds)
           (receive new-seeds (apply dir-combiner dir-entry seeds)
             (apply values #t #t #f new-seeds)))
         seeds))

;;;@subheading Unsorted

;;@ Create directories, with intermediary ones when needed.
(define (create-directory* pathname)
  (let ((pathname (pathname-as-directory pathname)))
    (fold (lambda (new path)
            (let ((new-dir (merge-pathnames (make-pathname #f (list new) #f)
                                            path)))
              (or (file-exists? new-dir) (create-directory new-dir))
              new-dir))
          (make-pathname (pathname-origin pathname) '() #f)
          (pathname-directory pathname))))

;;@ Search @var{dir-list}, a list of directories for an occurance of a
;; file as specified by @var{pathname}. If @var{pred} is specified, it
;; must be a single-argument procedure, which is used as an additional
;; predicate that must be satisfied.
(define find-file
  (case-lambda
    ((pathname dir-list pred)
     (let ((pathname (->pathname pathname)))
       (cond ((not (pathname-origin pathname))
              (let loop ((lst dir-list))
                (if (null? lst)
                    #f
                    (let ((path (pathname-join
                                 (pathname-as-directory (car lst))
                                 pathname)))
                      (if (and (file-exists? path)
                               (or (not pred) (pred path)))
                          path
                          (loop (cdr lst)))))))
             ((and (file-exists? pathname)
                   (or (not pred) (pred pathname)))
              pathname)
             (else
              #f))))
    ((pathname dir-list)
     (find-file pathname dir-list #f))))

;;@ Vanilla file installation procedure that simply copies the
;; file, creating any needed directory.
(define (install-file src dest)
  (create-directory* dest)
  (copy-file src dest))

(define (copy-file src-pathname dst-pathname)
  (call-with-port (open-file-input-port (->namestring src-pathname))
    (lambda (in-port)
      (call-with-output-file/atomic dst-pathname 'block #f
        (lambda (out-port)
          (copy-port in-port out-port))))))

;;@ Call @var{proc}, with the a file input port corresponding to @1,
;;with a working directory as specified by the directory part of @1.
(define (call-with-input-file-and-directory pathname proc)
  (let ((pathname (->pathname pathname)))
    (with-working-directory (pathname-with-file pathname #f)
      (lambda ()
        (call-with-input-file (file-namestring pathname) proc)))))

;;@ Call @var{proc}, with a file output port corresponding to a
;; temporary file. When @var{proc} returns normally, the temporary
;; file is renamed to @var{pathname}, which normally is an atomic
;; operation.
(define call-with-output-file/atomic
  (case-lambda
    ((pathname buffer-mode transcoder proc)
     (receive (tmp-filename tmp-port)
              (create-temp-file pathname buffer-mode transcoder)
       (guard (c (#t
                  (close-port tmp-port)
                  (delete-file tmp-filename)
                  (raise c)))
         (receive results (call-with-port tmp-port proc)
           (rename-file tmp-filename pathname)
           (apply values results)))))
    ((pathname buffer-mode proc)
     (call-with-output-file/atomic pathname buffer-mode #f proc))
    ((pathname proc)
     (call-with-output-file/atomic pathname 'block (native-transcoder) proc))))

;;;@subheading Temporary files

;; The procedures in this section can be used to create temporary
;; files with unique names.  They all accept a @var{template}
;; argument, which may either be a pathname, or a procedure of one
;; argument.  If it is a pathname, the pathname's file name component
;; is treated as a textual template which may contain placeholders as
;; used with @ref{(spells string-utils)
;; string-substitute,,string-substitute}.  The following subsitutions
;; are available:
;;
;; @table @code
;; @item count
;; Will be subsituted with the iteration counter.
;; @item random
;; Will be subsituted with a random alphabetical string.
;; @item pid
;; Will be substituted with the current operating process' identifier
;; (usually an integer).
;; @end table
;;
;; If @var{template} is a procedure, it is invoked with the iteration
;; counter, and expected to return a pathname.

;;@ Create a temporary file.  This procedure returns two values: the
;; pathname of the created file, and an output port corresponding to
;; that pathname.
(define create-temp-file
  (case-lambda
    ((template buffer-mode transcoder)
     (temp-pathname-iterate
      (lambda (pathname continue)
        (let ((port (open-file-output-port (->namestring pathname)
                                           (file-options)
                                           buffer-mode
                                           transcoder)))
          (values pathname port)))
      template))
    ((template buffer-mode)
     (create-temp-file template buffer-mode #f))
    ((template)
     (create-temp-file template 'block (native-transcoder)))
    (()
     (create-temp-file (default-temp-template) 'block (native-transcoder)))))

;;@ Create a temporary directory and return its pathname.
(define create-temp-directory
  (case-lambda
    ((template)
     (temp-pathname-iterate
      (lambda (pathname continue)
        (let ((directory (pathname-as-directory pathname)))
          (create-directory directory)
          directory))
      template))
    (()
     (create-temp-directory (default-temp-template)))))

;;@ Generator for temporary pathnames.  Its argument @var{action}
;; should be a procedure that attempts to open a file, the pathname of
;; which will be passed to it as first argument.  If the invocation of
;; @var{action} raises an @code{&i/o-file-already-exists} condition, a
;; new pathname is created, and @var{action} is called again with that
;; pathname.  In addition to raising an
;; @code{&i/o-file-already-exists} condition, @var{action} can use its
;; second argument, which is a thunk, to decline the pathname it is
;; provided with.
;;
;; The optional second argument, @var{template}, is used to generate
;; the pathnames that @var{action} is invoked with.
(define temp-pathname-iterate
  (case-lambda
    ((action template)
     (let ((pathname-generator
            (if (procedure? template)
                template
                (make-pathname-generator template))))
       (let loop ((i 1))
         (guard (c ((i/o-file-already-exists-error? c)
                    (loop (+ i 1))))
           (action (pathname-generator i) (lambda () (loop (+ i 1))))))))
    ((action)
     (temp-pathname-iterate action (default-temp-template)))))

;;@stop

(define (make-pathname-generator template)
  (lambda (count)
    (let* ((template (->pathname template))
           (file (pathname-file template))
           (types (if file (file-types file) '()))
           (fname (and file (file-name file))))
      (pathname-with-file
       template
       (make-file (string-substitute
                   (if (and fname
                            (exists (lambda (variable)
                                      (string-contains fname variable))
                                    '("{count}" "{random}")))
                       fname
                       (string-concatenate (append
                                            (if fname (list fname "-") '())
                                            '("{pid}-{random}.tmp"))))
                   `((count . ,count)
                     (random . ,(create-random-string 6))
                     (pid . ,(get-process-id))))
                  types)))))

(define default-temp-template
  (let ((cache #f))
    (lambda ()
      (unless cache
        (set! cache (pathname-as-directory
                     (or (get-environment-variable "TMPDIR") "/var/tmp/"))))
      cache)))

(define (create-random-string len)
  (string-unfold (lambda (seed)
                   (= len (cdr seed)))
                 car
                 (lambda (seed)
                   (cons (random-char) (+ 1 (cdr seed))))
                 (cons (random-char) 0)))

(define random-char
  (let ((alphabet
         "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
    (lambda ()
      (string-ref alphabet (filename-random-integer (string-length alphabet))))))

(define-condition-type &file-unreachable-error &error
  file-unreachable-error? make-file-unreachable-error
  (pathname file-unreachable-error-pathname)
  (operator file-unreachable-error-operator))

(define filename-random-source (make-random-source))
(define filename-random-integer (random-source-make-integers filename-random-source))

(random-source-randomize! filename-random-source)

)
