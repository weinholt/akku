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
    (rename (chdir cd))
    getcwd
    mkdir
    chmod
    rename-file
    (rename (rmdir delete-directory))
    symlink
    readlink
    putenv
    open-process-ports
    directory-list
    ;; Single-argument versions
    file-exists/no-follow? file-symbolic-link?
    file-regular? file-directory?       ;follow symlinks
    pretty-print
    get-passwd-realname
    os-name)
  (import
    (except (rnrs (6)) file-exists?)
    (only (ice-9 pretty-print) pretty-print)
    (only (guile)
          chdir getcwd
          mkdir chmod rename-file rmdir
          lstat stat stat:type
          symlink readlink
          opendir readdir closedir
          string-split passwd:gecos getpwnam getlogin
          uname utsname:sysname)
    (prefix (only (guile) putenv) guile:)
    (only (srfi :13 strings) string-prefix?)
    (prefix (spells process) spells:))

(define (file-exists/no-follow? fn)
  (guard (exn ((and (who-condition? exn)
                    (member (condition-who exn) '("lstat" lstat)))
               #f))
    (lstat fn)
    #t))

(define (file-regular? fn)
  (guard (exn ((and (who-condition? exn)
                    (member (condition-who exn) '("stat" stat)))
               #f))
    (eq? (stat:type (stat fn)) 'regular)))

(define (file-directory? fn)
  (guard (exn ((and (who-condition? exn)
                    (member (condition-who exn) '("stat" stat)))
               #f))
    (eq? (stat:type (stat fn)) 'directory)))

(define (file-symbolic-link? fn)
  (guard (exn ((and (who-condition? exn)
                    (member (condition-who exn) '("lstat" lstat)))
               #f))
    (eq? (stat:type (lstat fn)) 'symlink)))

(define (directory-list dirname)
  (let ((dir (opendir dirname)))
    (do ((entry (readdir dir) (readdir dir))
         (ret '() (if (member entry '("." ".."))
                      ret
                      (cons entry ret))))
        ((eof-object? entry)
         (closedir dir)
         ret))))

(define (open-process-ports cmd buffer-mode transcoder)
  (let ((p (spells:spawn-process #f #f #f #f "/bin/sh" "-c" cmd)))
    (values (transcoded-port (spells:process-input p) transcoder)
            (transcoded-port (spells:process-output p) transcoder)
            (transcoded-port (spells:process-errors p) transcoder)
            (spells:process-id p))))

(define (putenv name value)
  (guile:putenv (string-append name "=" value)))

(define (get-passwd-realname)
  (car (string-split (passwd:gecos (getpwnam (getlogin)))
                     #\,)))

(define (os-name)
  (let ((os (utsname:sysname (uname))))
    (cond ((string-prefix? "CYGWIN_NT-" os)
           'cygwin)
          ((string-prefix? "MSYS_NT-" os)
           'msys)
          (else
           (string->symbol (string-downcase os)))))))
