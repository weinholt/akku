;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2017-2019 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: GPL-3.0-or-later
#!r6rs

;; Definition of the manifest format.

(library (akku format manifest)
  (export
    manifest-filename
    akku-package)
  (import
    (rnrs)
    (for (semver versions) expand))

(define manifest-filename "Akku.manifest")

(define-syntax akku-package
  (lambda (x)
    (syntax-case x (synopsis description authors license notice-files extra-files
                             source depends depends/dev)
      ((_ (name version) . x*)
       #'(begin
           (check-name name)
           (check-version version)
           #f)))))

(define-syntax check-name
  (lambda (x)
    (syntax-case x ()
      ((_ (id ...))
       (for-all (lambda (component)
                  (or (symbol? (syntax->datum component))
                      (number? (syntax->datum component))))
                #'(id ...)))
      ((_ name)
       (string? (syntax->datum #'name))))))

(define-syntax check-version
  (lambda (x)
    (syntax-case x ()
      ((_ version)
       (guard (exn
               ((and (who-condition? exn) (eq? (condition-who exn) 'string->semver))
                (syntax-violation 'check-version "Not a valid SemVer" #'version)))
         (string->semver (syntax->datum #'version)))
       #f))))

;; (define-syntax license
;;   (lambda (x)
;;     (syntax-case x ()
;;       ((_ spdx-id)
;;        (string? (syntax->datum #'spdx-id))))))

;; (define-syntax private
;;   (lambda (x)
;;     (syntax-case x ()
;;       ((_)
;;        #'#f))))

;; (define-syntax depends
;;   (lambda (x)
;;     (syntax-case x ()
;;       ((_ . pkg*) #'#f))))

;; (define-syntax depends/dev
;;   (lambda (x)
;;     (syntax-case x ()
;;       ((_ . pkg*) #'#f))))
)
