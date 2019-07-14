;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2017-2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

;; Definition of the lockfile format.

(library (akku format lockfile)
  (export
    lockfile-filename
    projects)
  (import
    (rnrs))

(define lockfile-filename "Akku.lock")

(define-syntax projects
  (lambda (x)
    (define (unique-names? name*)
      (or (null? name*)
          (and (not (memp (lambda (name) (string-ci=? name (car name*))) (cdr name*)))
               (unique-names? (cdr name*)))))

    (syntax-case x (name)
      ((_ ((name name*) attr* ...) ...)
       (unique-names? (syntax->datum #'(name* ...)))
       #'(begin
           (write `(projects ,(check-project ((name name*) attr* ...))
                             ...))
           (newline))))))

(define-syntax check-project
  (lambda (x)
    (syntax-case x (name)
      ((_ ((name project-name)
           attr* ...))
       #'`((name project-name) ,(check-attr attr*) ...)))))

(define-syntax check-attr
  (lambda (x)
    (syntax-case x (location git directory tag revision)
      ((_ (location (git url)))
       (string? (syntax->datum #'url))
       #''(location (git url)))
      ((_ (location (directory path)))
       (string? (syntax->datum #'path))
       #''(location (directory path)))
      ((_ (tag tag-name))
       (string? (syntax->datum #'tag-name))
       #''(tag tag-name))
      ((_ (revision commit-id))
       (string? (syntax->datum #'commit-id))
       #''(revision commit-id))))))
