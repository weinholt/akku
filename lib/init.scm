;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2018, 2019 Göran Weinholt <goran@weinholt.se>
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

;; Initialize a new project with a template.

(library (akku lib init)
  (export
    init-project)
  (import
    (except (rnrs (6)) call-with-output-file)
    (srfi :19 time)
    (srfi :39 parameters)
    (industria strings)
    (wak fmt)
    (only (akku format manifest) manifest-filename)
    (only (akku private compat) chmod file-directory?)
    (only (akku lib manifest) default-manifest-name
          default-manifest-version default-manifest-license)
    (only (akku lib lock) add-dependency)
    (only (akku lib utils) mkdir/recursive path-join split-path
          get-realname get-index-filename)
    (akku private logging))

(define logger:akku.init (make-logger logger:akku 'init))
(define log/info (make-fmt-log logger:akku.init 'info))
(define log/warn (make-fmt-log logger:akku.init 'warning))
(define log/debug (make-fmt-log logger:akku.init 'debug))
(define log/trace (make-fmt-log logger:akku.init 'trace))
(define log/critical (make-fmt-log logger:akku.init 'critical))

(define (call-with-output-file filename proc) ;workaround for Guile 2.2.4
  (call-with-port (open-output-file filename) proc))

(define MIT-license
  "Permission is hereby granted, free of charge, to any person obtaining a
copy of this software and associated documentation files (the \"Software\"),
to deal in the Software without restriction, including without limitation
the rights to use, copy, modify, merge, publish, distribute, sublicense,
and/or sell copies of the Software, and to permit persons to whom the
Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
DEALINGS IN THE SOFTWARE.")

(define (init-project dirname)
  (define (program-header)
    (cat "#!/usr/bin/env scheme-script" nl
         ";; -*- mode: scheme; coding: utf-8 -*- !#" nl
         (header)))
  (define (library-header)
    (cat ";; -*- mode: scheme; coding: utf-8 -*-" nl
         (header)))
  (define (copyright)
    (cat "Copyright (c) " (date-year (current-date)) " " (get-realname)))
  (define (header)
    (cat ";; " (copyright) nl
         ";; SPDX-License-Identifier: MIT" nl
         "#!r6rs" nl
         nl))

  (when (file-exists? dirname)
    (log/critical "The directory " (wrt dirname) " already exists.")
    (exit 1))
  (let ((name (cdr (split-path dirname))))
    (let ((bin-dir (path-join dirname "bin"))
          (lib-dir dirname)
          (tests-dir (path-join dirname "tests"))
          (libname `(,(string->symbol name))))
      (let ((bin (path-join bin-dir (string-append name ".sps")))
            (lib (path-join lib-dir (string-append name ".sls")))
            (test (path-join tests-dir (string-append "test-" name ".sps")))
            (gitignore (path-join dirname ".gitignore"))
            (license (path-join dirname "LICENSE")))
        (mkdir/recursive bin-dir)
        (mkdir/recursive lib-dir)
        (mkdir/recursive tests-dir)

        ;; Command-line program.
        (call-with-output-file bin
          (lambda (p)
            (fmt p (program-header)
                 (pretty `(import (rnrs (6))
                                  ,libname))
                 nl
                 (pretty '(display (hello "World")))
                 (pretty '(newline)))))

        ;; Simple library.
        (call-with-output-file lib
          (lambda (p)
            (fmt p (library-header)
                 "(library " (wrt libname) nl
                 "  (export hello)" nl
                 "  (import (rnrs))" nl
                 nl
                 "(define (hello whom)" nl
                 "  (string-append \"Hello \" whom \"!\")))" nl)))

        ;; An example unit test.
        (call-with-output-file test
          (lambda (p)
            (fmt p
                 (program-header)
                 (pretty `(import (rnrs (6))
                                  (srfi :64 testing)
                                  ,libname))
                 nl
                 (pretty '(test-begin "hello"))
                 (pretty '(test-equal "Hello World!" (hello "World")))
                 (pretty '(test-end))
                 nl
                 (pretty '(exit (if (zero? (test-runner-fail-count (test-runner-get)))
                                    0 1))))))

        ;; For completeness.
        (call-with-output-file gitignore
          (lambda (p)
            (fmt p ".akku" nl)))
        (call-with-output-file license
          (lambda (p)
            (fmt p (copyright) nl
                 "Valid-License-Identifier: MIT" nl
                 nl
                 MIT-license nl)))

        ;; Set permissions
        (chmod bin #o755)
        (chmod test #o755)

        ;; Make a sensible manifest that gets us SRFI-64.
        (parameterize ([default-manifest-name name]
                       [default-manifest-version "1.0.0-alpha.0"]
                       [default-manifest-license "MIT"])
          (add-dependency (path-join dirname manifest-filename)
                          (get-index-filename)
                          #f "chez-srfi" ">=0.0.0-akku <1.0.0")))))))
