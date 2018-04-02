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

;; Common utilities that should really be imported from elsewhere.

(library (akku lib utils)
  (export
    print
    append-map filter-map map-in-order delete-duplicates
    string-prefix? string-suffix? string-index
    string-split
    mkdir/recursive split-path path-join (rename (path-join url-join))
    read-shebang
    pipe-ports
    application-home-directory
    cache-directory
    running-from-home?
    sanitized-name
    get-terminal-size)
  (import
    (rnrs (6))
    (rnrs mutable-pairs (6))
    (only (srfi :1 lists) append-map filter-map map-in-order delete-duplicates)
    (only (srfi :13 strings) string-prefix? string-suffix? string-index string-trim-right)
    (only (industria strings) string-split)
    (only (akku lib compat) getcwd file-directory? mkdir getenv))

(define (print . x*)
  (for-each (lambda (x)
              (display x (current-error-port)))
            x*)
  (newline (current-error-port)))

;; Split directory name and filename components.
(define (split-path filename)
  (let-values (((p extract) (open-string-output-port)))
    (let lp ((part* (string-split filename #\/)))
      (cond ((null? (cdr part*))
             (cons (extract) (car part*)))
            (else
             (put-string p (car part*))
             (unless (null? (cddr part*))
               (put-char p #\/))
             (lp (cdr part*)))))))

;; Join two path components.
(define (path-join x y)
  (cond ((string-suffix? "/" x)
         (string-append x y))
        ((string=? x "")
         y)
        ((string-prefix? "/" y)
         y)
        (else
         (string-append x "/" y))))

(define (mkdir/recursive path)
  (unless (file-directory? path)
    (let ((component* (string-split path #\/)))
      (let lp ((component* (cdr component*))
               (dir (if (string-prefix? "/" path)
                        "/"
                        (car component*))))
        (unless (file-directory? dir)
          (mkdir dir))
        (unless (null? component*)
          (lp (cdr component*) (path-join dir (car component*))))))))

(define (read-shebang port)
  (let ((start (port-position port)))
    (let ((line1 (get-line port)))
      (cond ((or (string-prefix? "#! " line1)
                 (string-prefix? "#!/" line1))
             line1)
            (else
             (set-port-position! port start)
             #f)))))

;; Copy all data to the port `outp` from `inp`.
(define (pipe-ports outp inp)
  (if (textual-port? outp)
      (let lp ()
        (let ((buf (get-string-n inp (* 16 1024))))
          (unless (eof-object? buf)
            (put-string outp buf)
            (lp))))
      (let lp ()
        (let ((buf (get-bytevector-n inp (* 16 1024))))
          (unless (eof-object? buf)
            (put-bytevector outp buf)
            (lp))))))

(define (application-home-directory)
  (cond ((getenv "AKKU_HOME"))
        (else
         (assert (getenv "HOME"))
         (path-join (getenv "HOME") ".akku"))))

(define (cache-directory)
  (cond ((getenv "AKKU_CACHE_DIR"))
        (else
         (assert (getenv "HOME"))
         (path-join (getenv "HOME") ".cache/akku"))))

(define (running-from-home?)
  (equal? (string-trim-right (getcwd) #\/)
          (string-trim-right (getenv "HOME") #\/)))

(define (sanitized-name name)
  ;; Turns a project/package name into that works as a directory/file name.
  (define hex "0123456789abcdefgh")
  (let ((dirname (if (string? name)
                     name
                     (call-with-string-output-port
                       (lambda (p) (display name p))))))
    (call-with-string-output-port
      (lambda (p)
        (do ((bv (string->utf8 (string-normalize-nfc dirname)))
             (i 0 (fx+ i 1)))
            ((fx=? i (bytevector-length bv)))
          (let* ((b (bytevector-u8-ref bv i))
                 (c (integer->char b)))
            (cond ((and (char>=? c #\space) (char<? c #\delete)
                        (not (string-index "<>:\"/\\|?*~" c)))
                   (put-char p c))
                  (else
                   (let-values (((n0 n1) (fxdiv-and-mod b 16)))
                     (put-char p #\%)
                     (put-char p (string-ref hex n0))
                     (put-char p (string-ref hex n1)))))))))))

(define (get-terminal-size)
  ;; TODO: implement properly.
  (values 80 24)))
