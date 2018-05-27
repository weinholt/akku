#!r6rs #!r6rs
;;; compat.guile.sls --- filesys compat library for Guile.

;; Copyright (C) 2009, 2010, 2011, 2012, 2015 Andreas Rottmann <a.rottmann@gmx.at>
;; Copyright (C) 2018 Göran Weinholt <goran@weinholt.se>

;; Authors: Andreas Rottmann <a.rottmann@gmx.at>
;;          Göran Weinholt <goran@weinholt.se>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;; This currently does not work on anything except Linux, due to
;; stat() being a hot mess.

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
          (rnrs records syntactic)
          (prefix (rnrs files) rnrs:)
          (srfi :8 receive)
          (spells pathname)
          (spells time-lib)
          (prefix (chezscheme) cs:)
          (only (chezscheme) file-regular?
                file-directory? file-symbolic-link?))

  (define dummy
    (case (cs:machine-type)
      ((i3le ti3le a6le ta6le arm32le ppc32le) (cs:load-shared-object "libc.so.6"))
      ((i3osx ti3osx a6osx ta6osx) (cs:load-shared-object "libc.dylib"))
      ((i3nt ti3nt a3nt ta3nt) (cs:load-shared-object "crtdll.dll"))
      (else (cs:load-shared-object "libc.so"))))

  (define ->fn ->namestring)

  (define (file-exists? pathname)
    (rnrs:file-exists? (->fn pathname)))

  (define (create-directory pathname)
    (let ((fn (->fn pathname)))
      (cs:mkdir fn)))

  (define create-symbolic-link
    (let ((%symlink (cs:foreign-procedure "symlink" (string string) int)))
      (lambda (old-pathname new-pathname)
        (let ((new-filename (->fn new-pathname)))
          ;; TODO: check errors
          (%symlink (->fn old-pathname) new-filename)))))

  (define create-hard-link
    (let ((%link (cs:foreign-procedure "link" (string string) int)))
      (lambda (old-pathname new-pathname)
        (let ((old-filename (->fn old-pathname))
              (new-filename (->fn new-pathname)))
          ;; TODO: check errors
          (let ((status (%link old-filename new-filename)))
            ;; (cs:display (list old-filename new-filename status)) (cs:newline)
            (unless (zero? status)
              (error 'create-hard-link "Error from libc" status)))))))

  (define (delete-file pathname)
    (let ((fname (->fn pathname)))
      (if (file-directory? fname)
          (cs:delete-directory fname #t)
          (cs:delete-file fname #t))))

  (define (rename-file source-pathname target-pathname)
    (let ((source-filename (->fn source-pathname))
          (target-filename (->fn target-pathname)))
      (cs:rename-file source-filename target-filename)))

  ;; From unistd.h on Linux
  (define R_OK 4)
  (define W_OK 2)
  (define X_OK 1)
  (define F_OK 0)

  (define make-file-check
    (let ((%access (cs:foreign-procedure "access" (string int) int)))
      (lambda (permission)
        (lambda (pathname)
          (let ((filename (->fn pathname)))
            (zero? (%access filename permission)))))))

  (define file-readable? (make-file-check R_OK))
  (define file-writable? (make-file-check W_OK))
  (define file-executable? (make-file-check X_OK))

  (define (file-modification-time pathname)
    (let ((t (cs:file-modification-time (->fn pathname))))
      (posix-timestamp->time-utc (cs:time-second t)
                                 (cs:time-nanosecond t))))

  (define file-size-in-bytes
    (let-values (((%stat statbuf-size st_size:offset st_size:type)
                  (case (cs:machine-type)
                    ((a6le ta6le)
                     (let ((stat (cs:foreign-procedure "__xstat64" (int string void*) int)))
                       (values (lambda (pathname statbuf) (stat 1 pathname statbuf))
                               144 48 'unsigned-64)))
                    ((arm32le tarm32le)
                     (let ((stat (cs:foreign-procedure "__xstat64" (int string void*) int)))
                       (values (lambda (pathname statbuf) (stat 3 pathname statbuf))
                               104 48 'unsigned-64)))
                    (else
                     (values #f #f #f #f)))))
      (lambda (pathname)
        (when (not %stat)
          (error 'file-size-in-bytes "TODO: stat implementation for this machine-type"
                 (cs:machine-type)))
        (let* ((&statbuf (cs:foreign-alloc statbuf-size))
               (status (%stat (->fn pathname) &statbuf)))
          (unless (zero? status)        ;TODO: check errors
            (error 'file-size-in-bytes "Error from libc" status))
          (let ((size (cs:foreign-ref st_size:type &statbuf st_size:offset)))
            (cs:foreign-free &statbuf)
            size)))))

;;; directory stream

  (define-record-type directory-stream
    (fields (mutable entries)))

  (define (open-directory-stream pathname)
    (make-directory-stream (cs:directory-list (->fn pathname))))

  (define (close-directory-stream stream)
    (directory-stream-entries-set! stream #f))

  (define (read-directory-stream stream)
    (let ((entries (directory-stream-entries stream)))
      (if (null? entries)
          #f
          (let ((filename (car entries)))
            (directory-stream-entries-set! stream (cdr entries))
            filename))))

;;; current directory

  (define (working-directory)
    (pathname-as-directory (cs:current-directory)))

  (define (with-working-directory dir thunk)
    (let ((wd (cs:current-directory)))
      (dynamic-wind
        (lambda () (cs:current-directory
                    (->fn (pathname-as-directory (->pathname dir)))))
        thunk
        (lambda () (cs:current-directory wd)))))

  (define (library-search-paths)
    (append (map (lambda (dir) (pathname-as-directory (car dir)))
                 (cs:library-directories))
            (map pathname-as-directory (cs:source-directories)))))
