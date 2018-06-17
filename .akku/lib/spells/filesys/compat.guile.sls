#!r6rs #!r6rs
;;; compat.guile.sls --- filesys compat library for Guile.

;; Copyright (C) 2010, 2011, 2012, 2015 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:


(library (spells filesys compat)
  (export file-exists?
          create-directory
          create-symbolic-link
          create-hard-link
          delete-file
          rename-file

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
          
          working-directory
          with-working-directory

          library-search-paths)
  (import (rnrs base)
          (rnrs lists)
          (rnrs conditions)
          (rnrs exceptions)
          (rnrs io ports)
          (rnrs control)
          (prefix (rnrs files) rnrs:)
          (srfi :8 receive)
          (spells pathname)
          (spells time-lib)
          (prefix (guile) guile:))

  (define ->fn ->namestring)

  (define i/o-error-constructors
    (list (cons guile:ENOENT make-i/o-file-does-not-exist-error)
          (cons guile:EEXIST make-i/o-file-already-exists-error)
          (cons guile:EACCES make-i/o-file-protection-error)))
  
  (define (with-exception-converter filename-chooser thunk)
    (guile:catch 'system-error
      thunk
      (lambda (key func fmt fmtargs data)
        (let ((errno (car data)))
          (define (construct-condition constructor)
            (apply condition
                   (append (list
                            (constructor (if (procedure? filename-chooser)
                                             (filename-chooser errno)
                                             filename-chooser)))
                           (if func
                               (list (make-who-condition func))
                               '())
                           (list (make-message-condition
                                  (apply guile:simple-format #f fmt fmtargs))))))
          (cond ((assv errno i/o-error-constructors)
                 => (lambda (pair)
                      (raise (construct-condition (cdr pair)))))
                (else
                 (raise (construct-condition make-i/o-filename-error))))))))
  
  (define (file-exists? pathname)
    (rnrs:file-exists? (->fn pathname)))

  (define (create-directory pathname)
    (let ((fn (->fn pathname)))
      (with-exception-converter fn (lambda () (guile:mkdir fn)))))

  (define (create-symbolic-link old-pathname new-pathname)
    (let ((new-filename (->fn new-pathname)))
      (with-exception-converter new-filename
        (lambda () (guile:symlink (->fn old-pathname) new-filename)))))

  (define (filename-chooser source-filename target-filename)
    (lambda (errno)
      (cond ((= errno guile:EEXIST)
             target-filename)
            (else
             source-filename))))

  (define (create-hard-link old-pathname new-pathname)
    (let ((old-filename (->fn old-pathname))
          (new-filename  (->fn new-pathname)))
      (with-exception-converter (filename-chooser old-filename new-filename)
        (lambda ()
          (guile:link old-filename new-filename)))))

  (define (delete-file pathname)
    (let ((fname (->fn pathname)))
      (with-exception-converter fname
        (lambda ()
          (let ((type (guile:catch 'system-error
                        (lambda () (guile:stat:type (guile:lstat fname)))
                        (lambda (key . args) #f))))
            (when type
              (case type
                ((directory) (guile:rmdir fname))
                (else        (guile:delete-file fname)))))))))

  (define (rename-file source-pathname target-pathname)
    (let ((source-filename (->fn source-pathname))
          (target-filename  (->fn target-pathname)))
      (with-exception-converter (filename-chooser source-filename target-filename)
        (lambda ()
          (guile:rename-file source-filename target-filename)))))

  (define (make-stat-type-checker type)
    (lambda (pathname)
      (let ((filename (->fn pathname)))
        (guile:catch 'system-error
          (lambda () (eq? type (guile:stat:type (guile:lstat filename))))
          (lambda (key . args)
            #f)))))
  
  (define file-regular? (make-stat-type-checker 'regular))
  (define file-directory? (make-stat-type-checker 'directory))
  (define file-symbolic-link? (make-stat-type-checker 'symlink))
  
  (define (make-file-check permission)
    (lambda (pathname)
      (let ((filename (->fn pathname)))
        ;; Somewhat suboptimal and racy, we trigger an exception here
        ;; if the file does not exist
        (with-exception-converter filename
          (lambda () (guile:stat filename)))
        (guile:access? filename permission))))

  (define file-readable? (make-file-check guile:R_OK))
  (define file-writable? (make-file-check guile:W_OK))
  (define file-executable? (make-file-check guile:X_OK))

  (define (file-modification-time pathname)
    (let ((st (guile:stat (->fn pathname))))
      (posix-timestamp->time-utc (guile:stat:mtime st)
                                 (guile:stat:mtimensec st))))

  (define (file-size-in-bytes pathname)
    (guile:stat:size (guile:stat (->fn pathname))))


  (define directory-stream? guile:directory-stream?)

  (define (open-directory-stream pathname)
    (let ((fn (->fn pathname)))
      (with-exception-converter fn (lambda () (guile:opendir fn)))))
  
  (define close-directory-stream guile:closedir)

  (define (read-directory-stream stream)
    (let loop ()
      (let ((filename (guile:readdir stream)))
        (cond ((eof-object? filename)
               #f)
              ((or (string=? "." filename)
                   (string=? ".." filename))
               (loop))
              (else
               filename)))))
  
  (define (working-directory)
    (pathname-as-directory (guile:getcwd)))

  (define (with-working-directory dir thunk)
    (let ((wd (guile:getcwd)))
      (dynamic-wind
        (lambda () (guile:chdir
                    (->fn (pathname-as-directory (->pathname dir)))))
        thunk
        (lambda () (guile:chdir wd)))))

  (define (library-search-paths)
    (map pathname-as-directory guile:%load-path))

  )

;; Local Variables:
;; scheme-indent-styles: ((guile:catch 1))
;; End:
