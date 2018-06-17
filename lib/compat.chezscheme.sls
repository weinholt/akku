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

(library (akku lib compat)
  (export
    cd
    (rename (cd getcwd))
    mkdir
    chmod
    delete-directory
    symlink
    readlink
    putenv
    open-process-ports
    directory-list
    file-exists/no-follow?
    pretty-print)
  (import
    (except (rnrs (6)) file-exists?)
    (only (chezscheme) cd mkdir chmod putenv rename-file delete-directory
          system process open-process-ports directory-list
          file-regular? file-directory? file-symbolic-link?
          pretty-print file-exists?
          machine-type load-shared-object foreign-procedure))

  (define (file-exists/no-follow? filename)
    (file-exists? filename #f))

  (define (symlink from to)
    (define %symlink (foreign-procedure "symlink" (string string) int))
    (let ((ret (%symlink from to)))
      (when (= ret -1)
        (error 'symlink "Could not create symbolic link" from to))))

  (define (readlink pathname)
    (define %readlink (foreign-procedure "readlink" (string u8* size_t) ssize_t))
    (define buf (make-bytevector 255 0))
    (let ((ret (%readlink pathname buf (bytevector-length buf))))
      (if (= ret -1)
          (error 'readlink "Could not read symbolic link" pathname)
          (utf8->string
           (get-bytevector-n (open-bytevector-input-port buf) ret)))))

  (case (machine-type)
    ((i3le ti3le a6le ta6le arm32le tarm32le) (load-shared-object "libc.so.6"))
    ((i3osx ti3osx a6osx ta6osx) (load-shared-object "libc.dylib"))
    ((i3nt ti3nt a3nt ta3nt) (load-shared-object "crtdll.dll"))
    (else (load-shared-object "libc.so"))))
