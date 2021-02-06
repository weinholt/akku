#!r6rs
;;; compat.guile.sls --- filesys compat library for Guile.

;; Copyright (C) 2010, 2011, 2012, 2015 Andreas Rottmann <a.rottmann@gmx.at>
;; Copyright (C) 2020 Andreas Rottmann <a.rottmann@gmx.at>

;; Authors: Andreas Rottmann <a.rottmann@gmx.at>
;;          Göran Weinholt <goran@weinholt.se>

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
          (rnrs records syntactic)
          (prefix (rnrs files) rnrs:)
          (spells pathname)
          (spells time-lib)
          (srfi :19 time)
          (prefix (srfi :170 posix) posix:)
          (prefix (only (loko) library-directories) loko:))

  (define ->fn ->namestring)

  (define (file-exists? pathname)
    (rnrs:file-exists? (->fn pathname)))

  (define (create-directory pathname)
    (posix:create-directory (->fn pathname)))

  (define (create-symbolic-link old-pathname new-pathname)
    (posix:create-symlink (->fn old-pathname) (->fn new-pathname)))

  (define (create-hard-link old-pathname new-pathname)
    (posix:create-hard-link (->fn old-pathname) (->fn new-pathname)))

  (define (delete-file pathname)
    (let ((fname (->fn pathname)))
      (guard (exn
              ((eq? (condition-who exn) 'file-info)
               (values)))
        (if (posix:file-info-directory? (posix:file-info fname #f))
            (posix:delete-directory fname)
            (rnrs:delete-file fname)))))

  (define (rename-file source-pathname target-pathname)
    (posix:rename-file (->fn source-pathname) (->fn target-pathname)))

  (define (make-stat-type-checker predicate)
    (lambda (pathname)
      (let ((filename (->fn pathname)))
        (guard (exn
                ((eq? (condition-who exn) 'file-info) #f))
          (predicate (posix:file-info filename #f))))))

  (define file-regular? (make-stat-type-checker posix:file-info-regular?))
  (define file-directory? (make-stat-type-checker posix:file-info-directory?))
  (define file-symbolic-link? (make-stat-type-checker posix:file-info-symlink?))

  (define (make-file-check permission)
    (lambda (pathname)
      (let ((filename (->fn pathname)))
        (error 'make-file-check "Not implemented" permission)
        #;
        (posix:access? filename permission))))

  (define file-readable? (make-file-check 'R_OK))
  (define file-writable? (make-file-check 'W_OK))
  (define file-executable? (make-file-check 'X_OK))

  (define (file-modification-time pathname)
    (posix:file-info:mtime (posix:file-info (->fn pathname))))

  (define (file-size-in-bytes pathname)
    (posix:file-info:size (posix:file-info (->fn pathname))))

  (define-record-type directory-stream
    (fields dir))

  (define (open-directory-stream pathname)
    (let ((fn (->fn pathname)))
      (make-directory-stream (posix:open-directory fn))))

  (define (close-directory-stream stream)
    (posix:close-directory (directory-stream-dir stream)))

  (define (read-directory-stream stream)
    (let ((filename (posix:read-directory (directory-stream-dir stream))))
      (if (eof-object? filename)
          #f
          filename)))

  (define (working-directory)
    (pathname-as-directory (posix:current-directory)))

  (define (with-working-directory dir thunk)
    (let ((wd (posix:current-directory)))
      (dynamic-wind
        (lambda () (posix:set-current-directory!
                    (->fn (pathname-as-directory (->pathname dir)))))
        thunk
        (lambda () (posix:set-current-directory! wd)))))

  (define (library-search-paths)
    (map pathname-as-directory (loko:library-directories))))
