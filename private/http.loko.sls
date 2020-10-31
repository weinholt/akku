;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2018, 2019, 2020 Göran Weinholt <goran@weinholt.se>
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

;;; HTTP client.

;; Forks a curl process for now.

(library (akku private http)
  (export
    make-http-request http-request?
    http-request-method
    http-request-url
    open-http-request
    http-response?
    http-response-status
    http-response-port
    download-file)
  (import
    (rnrs (6))
    (only (srfi :13 strings) string-prefix?)
    (akku lib utils)
    (akku private compat)
    (akku private logging)
    (wak fmt)
    (pre-srfi processes)
    (loko system fibers))

(define logger:akku.http (make-logger logger:akku 'http))
(define log/info (make-fmt-log logger:akku.http 'info))
(define log/error (make-fmt-log logger:akku.http 'error))
(define log/warn (make-fmt-log logger:akku.http 'warning))
(define log/debug (make-fmt-log logger:akku.http 'debug))
(define log/trace (make-fmt-log logger:akku.http 'trace))

(define-record-type http-request
  (nongenerative)
  (sealed #t)
  (fields method url))

(define-record-type http-response
  (nongenerative)
  (sealed #t)
  (fields status port))

(define (read-http-header resp-ch port)
  (let* ((port (transcoded-port port (make-transcoder (utf-8-codec)
                                                      (eol-style crlf))))
         (status (string-split (get-line port) #\space)))
    ;; TODO: Check if curl always writes this status before the data,
    ;; or if data needs to be read for it to not block
    (spawn-fiber
     (lambda ()
       (get-string-all port)))
    (cadr status)))

(define (open-http-request req)
  (define url (http-request-url req))
  (define method (http-request-method req))
  (let-values ([(head-r head-w) (make-pipe)]
               [(data-r data-w) (make-pipe)])
    (log/debug method " " (wrt url))
    (let* ((setup (list 'path #t
                        'stdin #f
                        'stdout data-w
                        'wait #f
                        3 head-w))
           (curl (make-process setup "curl" "-s"
                               "-D" (string-append "/dev/fd/3")
                               "-X" (string-upcase (symbol->string method))
                               url)))
      (define (read! bv start count)
        (let ((n (get-bytevector-n! data-r bv start count)))
          (if (eof-object? n)
              0
              n)))
      (define (close!)
        (close-port head-r)
        (close-port data-r)
        (close-port head-r)
        (process-wait curl))
      (close-port data-w)
      (close-port head-w)
      (let ((data-port (make-custom-binary-input-port url read! #f #f close!))
            (status (read-http-header #f head-r)))
        (make-http-response status data-port)))))

(define (download-file url local-filename callback)
  (call-with-port (open-file-output-port local-filename)
    (lambda (p)
      (cond
        ((or (string-prefix? "http:" url)
             (string-prefix? "https:" url))
         (let* ((req (make-http-request 'get url))
                (resp (open-http-request req)))
           (unless (equal? (http-response-status resp) "200")
             (close-port (http-response-port resp))
             (error 'download-file "Bad http response" resp req))
           (let lp ()
             (let ((buf (get-bytevector-n (http-response-port resp)
                                          (* 64 1024))))
               (unless (eof-object? buf)
                 (put-bytevector p buf)
                 (when callback (callback buf))
                 (lp))))
           (close-port (http-response-port resp))))
        (else
         (error 'download-file "URL scheme not supported" url)))))))
