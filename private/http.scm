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

;; HTTP client.

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
    (pffi)
    (only (srfi :13 strings) string-prefix?)
    (akku lib utils)
    (akku private compat)
    (akku private logging))

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

(define (c-string->string ptr)
  (utf8->string
   (call-with-bytevector-output-port
     (lambda (p)
       (let lp ((i 0))
         (let ((c (pointer-ref-c-uint8 ptr i)))
           (unless (fxzero? c)
             (put-u8 p c)
             (lp (fx+ i 1)))))))))

(define (string->c-string str)
  (string->utf8 (string-append str "\x0;")))

(define open-http-request
  (let ((libcurl #f))
    (define CURLE_OK                      0)
    (define CURLOPT_URL               10002)
    (define CURLOPT_WRITEFUNCTION     20011)
    (define CURLOPT_VERBOSE              41)
    (define CURLOPT_HEADER               42)
    (define CURLOPT_NOPROGRESS           43)
    (define CURLOPT_FAILONERROR          45)
    (define CURLOPT_FOLLOWLOCATION       52)
    (define CURLOPT_HEADERFUNCTION    20079)
    (define CURLINFO_RESPONSE_CODE #x200002)
    (lambda (req)
      (define-syntax call
        (lambda (x)
          (syntax-case x ()
            ((_ proc args ...)
             (with-syntax (((tmps ...) (generate-temporaries #'(args ...))))
               #'(let ((tmps args) ...)
                   (let ((status (proc tmps ...)))
                     (unless (eqv? status CURLE_OK)
                       (error 'proc "Error from libcurl" status tmps ...)))))))))
      (unless libcurl
        ;; Initialize libcurl and get a handle
        (log/trace "Initializing libcurl")
        (case (os-name)
          ((darwin) (set! libcurl (open-shared-object "libcurl.dylib")))
          ((msys) (set! libcurl (open-shared-object "msys-curl-4")))
          (else
           (guard (exn
                   (else
                    (set! libcurl (open-shared-object "libcurl.so.3"))))
             (set! libcurl (open-shared-object "libcurl.so.4")))))
        (letrec ()
          (define %curl_global_init (foreign-procedure libcurl int curl_global_init (long)))
          (call %curl_global_init #b11)))
      (letrec ()
        (define %curl_easy_init (foreign-procedure libcurl pointer curl_easy_init ()))
        (define %curl_easy_setopt/long (foreign-procedure libcurl int curl_easy_setopt (pointer int long)))
        (define %curl_easy_setopt/pointer (foreign-procedure libcurl int curl_easy_setopt (pointer int pointer)))
        (define %curl_easy_setopt/callback (foreign-procedure libcurl int curl_easy_setopt
                                                              (pointer int (callback int (pointer int int pointer)))))
        (define %curl_easy_perform (foreign-procedure libcurl int curl_easy_perform (pointer)))
        (define %curl_easy_getinfo (foreign-procedure libcurl int curl_easy_getinfo (pointer int pointer)))
        (define %curl_easy_reset (foreign-procedure libcurl void curl_easy_reset (pointer)))
        (define %curl_easy_cleanup (foreign-procedure libcurl void curl_easy_cleanup (pointer)))
        (log/debug "Using libcurl to download " (http-request-url req))
        (let ((curl (%curl_easy_init)))
          (call %curl_easy_setopt/pointer curl CURLOPT_URL
                (bytevector->pointer (string->c-string (http-request-url req))))
          (call %curl_easy_setopt/long curl CURLOPT_VERBOSE
                (if (memq (get-log-threshold) '(debug trace)) 1 0))
          (call %curl_easy_setopt/long curl CURLOPT_NOPROGRESS 1)
          (call %curl_easy_setopt/long curl CURLOPT_FOLLOWLOCATION 1)
          (call %curl_easy_setopt/long curl CURLOPT_FAILONERROR 1)
          ;; Read the whole response into memory. The other options
          ;; are to use curl_easy_recv(), but it requires select(); or
          ;; do jump in and out of libcurl's callback with call/cc.
          ;; Files are small enough to fit in memory for now.
          (let ((bv (call-with-bytevector-output-port
                      (lambda (p)
                        (let ((cb (c-callback int (pointer int int pointer)
                                          (lambda (ptr size nmemb stream)
                                            (let ((bytes (* size nmemb)))
                                              (log/trace "curl read: " bytes)
                                              (do ((i 0 (fx+ i 1)))
                                                  ((fx=? i bytes))
                                                (put-u8 p (pointer-ref-c-uint8 ptr i)))
                                              bytes)))))
                          (call %curl_easy_setopt/callback curl CURLOPT_WRITEFUNCTION cb)
                          (call %curl_easy_perform curl)
                          (free-c-callback cb))))))
            (let ((resp-bv (make-bytevector 8 0)))
              (call %curl_easy_getinfo curl CURLINFO_RESPONSE_CODE (bytevector->pointer resp-bv))
              (make-http-response (number->string (pointer-ref-c-long (bytevector->pointer resp-bv) 0))
                                  (open-bytevector-input-port bv)))))))))

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
