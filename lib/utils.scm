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

;; Common utilities, a big bag of everything

(library (akku lib utils)
  (export
    append-map filter-map map-in-order delete-duplicates
    assq-ref assq-update assoc-replace
    string-prefix? string-suffix? string-index
    string-split
    mkdir/recursive split-path path-join (rename (path-join url-join))
    read-shebang
    pipe-ports
    application-data-directory system-data-directory
    system-project-directories
    cache-directory
    local-ignore-file
    running-from-home?
    sanitized-name
    get-terminal-size
    symlink/relative
    resolve-pathname
    resolve-relative-filename
    get-log-threshold
    run-command
    get-realname
    get-index-filename
    check-filename
    with-working-directory
    setting get-settings)
  (import
    (rnrs (6))
    (rnrs mutable-pairs (6))
    (only (srfi :1 lists) make-list append-map filter-map map-in-order delete-duplicates last)
    (only (srfi :13 strings) string-prefix? string-suffix? string-index string-trim-right
          string-join)
    (rename (srfi :98 os-environment-variables) (get-environment-variable getenv))
    (only (spells process) run-shell-command)
    (only (industria strings) string-split)
    (chibi match)
    (akku config)
    (only (akku private compat) cd getcwd mkdir symlink get-passwd-realname
          file-directory?))

(define assq-ref
  (case-lambda
    ((alist key)
     (cond ((assq key alist) => cdr)
           (else
            (error 'assq-ref "Key not found" key alist))))
    ((alist key default)
     (cond ((assq key alist) => cdr)
           (else default)))))

(define (assq-update alist key proc default)
  (cond ((assq key alist)
         (map (lambda (x)
                (if (eq? (car x) key)
                    (cons key (proc (cdr x)))
                    x))
              alist))
        (else
         (cons (cons key (proc default)) alist))))

(define (assoc-replace alist key value)
  (cond ((assoc key alist)
         (map (lambda (x)
                (if (equal? (car x) key)
                    (cons key value)
                    x))
              alist))
        (else
         (cons (cons key value) alist))))

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

(define (application-data-directory)
  (cond ((getenv "XDG_DATA_HOME") =>
         (lambda (dir)
           (path-join dir "akku")))
        (else
         (assert (getenv "HOME"))
         (path-join (getenv "HOME") ".local/share/akku"))))

(define (cache-directory)
  (cond ((getenv "XDG_CACHE_HOME") =>
         (lambda (dir)
           (path-join dir "akku")))
        (else
         (assert (getenv "HOME"))
         (path-join (getenv "HOME") ".cache/akku"))))

(define (local-ignore-file directory)
  (path-join directory ".akkuignore"))

;; System-wide data directory, e.g. /usr/share/akku
(define (system-data-directory)
  (cond ((install-prefix) =>
         (lambda (dir)
           (path-join dir "share/akku")))
        (else #f)))

(define (system-project-directories)
  (cond ((getenv "AKKU_PROJECTS") =>
         (lambda (dir)
           (if (equal? dir "")
               '()
               (string-split dir #\:))))
        (else '())))

(define (running-from-home?)
  (equal? (string-trim-right (getcwd) #\/)
          (string-trim-right (getenv "HOME") #\/)))

(define (sanitized-name name)
  ;; Turns a project/package name into something that works as a
  ;; directory/file name.
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
  (values 80 24))

;; A not fully-generic relative symlink utility, similar to ln -r in
;; GNU coreutils. It can be asked to symlink within .akku/lib or from
;; ./ to somewhere inside .akku. Basically it will never go outside
;; the current directory. The node being created is `to`.
(define symlink/relative
  (case-lambda
    ((from to)
     (symlink/relative from to #f))
    ((from to dry-run?)
     (define (relativize from to)
       (let lp ((from* (remove "." (string-split from #\/)))
                (to* (remove "." (string-split to #\/))))
         (cond ((equal? (car from*) (car to*))
                (lp (cdr from*) (cdr to*)))
               (else
                (let* ((levels-up (- (length to*) 1))
                       (path-walk (make-list levels-up "..")))
                  (path-join (string-join path-walk "/" 'infix)
                             (string-join from* "/" 'infix)))))))
     (let ((rel-from (relativize from to)))
       (unless dry-run?
         (symlink rel-from to))
       rel-from))))

;; Get rid of "." and ".." in the path.
(define (resolve-pathname path)
  (fold-left path-join
             ""
             (let lp ((components (string-split path #\/)))
               (match components
                 [("." . rest) (lp rest)]
                 [(".." . rest)
                  (assertion-violation 'resolve-pathname
                                       "Filename goes outside the repo" path)]
                 [(dir ".." . rest) (lp rest)]
                 [(x . y) (cons x (lp y))]
                 [() '()]))))

;; Find the absolute path of the file `to` which is referenced from
;; the file `from`.
(define (resolve-relative-filename from to)
  (resolve-pathname (path-join (car (split-path from))
                               to)))

;; Get the configured log threshold.
(define (get-log-threshold)
  (let ((level (getenv "AKKU_LOG_LEVEL")))
    (if level
        (string->symbol level)
        'info)))

(define (run-command cmd)
  (let-values (((status signal) (run-shell-command cmd)))
    (unless (eqv? status 0)
      (error 'run "Shell command returned error" cmd status signal))))

(define (get-realname)
  (get-passwd-realname))

(define (get-index-filename)
  (define (maybe-join x y)
    (and x (path-join x y)))
  (let ((index (path-join (application-data-directory) "index.db"))
        (bootstrap (path-join (application-data-directory) "bootstrap.db"))
        (sys-bootstrap (maybe-join (system-data-directory) "bootstrap.db")))
    (cond ((file-exists? index) index)
          ((file-exists? bootstrap) bootstrap)
          ((and sys-bootstrap (file-exists? sys-bootstrap)) sys-bootstrap)
          (else (error 'cmd-lock "Unable to locate the package index"
                       index bootstrap sys-bootstrap)))))

(define (check-filename filename windows?)
  ;; Protection against path traversal attacks and other types of
  ;; names that would break on some systems. For one particularly
  ;; difficult system, see:
  ;; https://msdn.microsoft.com/en-us/library/windows/desktop/aa365247(v=vs.85).aspx
  ;; TODO: Check for unicode names that break under Windows.
  (define reserved-names
    '("CON" "PRN" "AUX" "NUL" "COM1" "COM2" "COM3" "COM4" "COM5" "COM6" "COM7" "COM8"
      "COM9" "LPT1" "LPT2" "LPT3" "LPT4" "LPT5" "LPT6" "LPT7" "LPT8" "LPT9"))
  (define reserved-chars
    "<>:\"/\\|?*")
  (for-each
   (lambda (component)
     (cond ((string=? component "") ;// does nothing
            (error 'check-filename "Empty path component" filename))
           ((member component '("." ".."))
            (error 'check-filename "Path component is . or .." filename))
           ((string-index component #\nul)
            (error 'check-filename "Path component contains NUL" filename))
           ((and windows?
                 (exists (lambda (part)
                           (exists (lambda (reserved) (string-ci=? part reserved))
                                   reserved-names))
                         (string-split component #\.)))
            (error 'check-filename "Path contains component reserved on MS Windows"
                   filename))
           ((and windows?
                 (exists (lambda (c) (string-index reserved-chars c))
                         (string->list component)))
            (error 'check-filename "Path contains character reserved on MS Windows"
                   filename))
           ((and windows?
                 (exists (lambda (c) (<= 1 (char->integer c) 31))
                         (string->list component)))
            (error 'check-filename "Path contains character disallowed on MS Windows"
                   filename))))
   (string-split filename #\/)))

;; Temporarily change working directories while running the thunk.
(define (with-working-directory dir thunk)
  (let ((cwd (getcwd)))
    (dynamic-wind (lambda () (cd dir))
                  thunk
                  (lambda () (cd cwd)))))

(define-enumeration setting
  (no-network no-dependencies)
  settings)

(define (get-settings)
  (cond ((getenv "AKKU_SETTINGS") =>
         (lambda (val)
           (enum-set-projection (make-enumeration
                                 (map string->symbol (string-split val #\,)))
                                (settings))))
        (else (settings)))))
