;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2017-2020 Göran Weinholt <goran@weinholt.se>
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

(library (akku private compat)
  (export
    (rename (set-current-directory! cd)
            (current-directory getcwd)
            (create-directory mkdir)
            (set-file-mode chmod)
            (create-symlink symlink)
            (read-symlink readlink))
    rename-file
    delete-directory
    directory-list

    putenv
    open-process-ports

    ;; Single-argument versions
    file-exists/no-follow? file-symbolic-link?
    file-regular? file-directory?       ;follow symlinks

    pretty-print
    get-passwd-realname
    os-name)
  (import
    (except (rnrs (6)) file-exists?)
    (srfi :170 posix)
    (only (srfi :170 compat) uname uname:os-name)
    (prefix (spells process) spells:)
    (only (loko) putenv pretty-print))

(define (directory-list dir)
  (directory-files dir))

(define (file-exists/no-follow? fn)
  (guard (exn ((and (who-condition? exn) (eqv? (condition-who exn) 'file-info))
               #f))
    (file-info fn #f)
    #t))

(define (file-regular? fn)
  (guard (exn ((and (who-condition? exn) (eqv? (condition-who exn) 'file-info))
               #f))
    (file-info-regular? (file-info fn #t))))

(define (file-directory? fn)
  (guard (exn ((and (who-condition? exn) (eqv? (condition-who exn) 'file-info))
               #f))
    (file-info-directory? (file-info fn #t))))

(define (file-symbolic-link? fn)
  (guard (exn ((and (who-condition? exn) (eqv? (condition-who exn) 'file-info))
               #f))
    (file-info-symlink? (file-info fn #f))))

(define (open-process-ports cmd buffer-mode transcoder)
  (let ((p (spells:spawn-process #f #f #f #f "/bin/sh" "-c" cmd)))
    (values (transcoded-port (spells:process-input p) transcoder)
            (transcoded-port (spells:process-output p) transcoder)
            (transcoded-port (spells:process-errors p) transcoder)
            (spells:process-id p))))

(define (get-passwd-realname)
  (guard (exn ((and (who-condition? exn)
                    (and (eq? 'user-info (condition-who exn))))
               #f))
    (let ((parts (user-info:parsed-full-name (user-info (user-uid)))))
      (and (pair? parts)
           (car parts)))))

(define (os-name)
  (let ((os (uname:os-name (uname))))
    (string->symbol (string-downcase os)))))
