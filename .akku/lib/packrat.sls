#!r6rs ;; Packrat Parser Library
;;
;; Copyright (c) 2004, 2005 Tony Garnock-Jones <tonyg@kcbbs.gen.nz>
;; Copyright (c) 2005 LShift Ltd. <query@lshift.net>
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;; SPDX-License-Identifier: MIT

(library (packrat)
  (export parse-result?
          parse-result-successful?
          parse-result-semantic-value
          parse-result-next
          parse-result-error

          parse-results?
          parse-results-position
          parse-results-base
          parse-results-next

          parse-error?
          parse-error-position
          parse-error-expected
          parse-error-messages
          parse-error->list

          make-parse-position
          parse-position?
          parse-position-file
          parse-position-line
          parse-position-column

          top-parse-position
          update-parse-position
          parse-position->string

          ;;empty-results
          ;;make-results

          make-error-expected
          make-error-message
          make-result
          parse-error->parse-result
          make-expected-result
          make-message-result

          prepend-base
          prepend-semantic-value

          base-generator->results
          results->result

          parse-position>?
          parse-error-empty?
          merge-parse-errors
          merge-result-errors

          parse-results-token-kind
          parse-results-token-value

          packrat-check-base
          packrat-check
          packrat-or
          packrat-unless

          packrat-parser
          packrat-lambda
          packrat-lambda*
          packrat-parse
          try-packrat-parse-pattern

          packrat-port-results
          packrat-string-results
          packrat-list-results

          <- quote ! ^ /)

  (import
    (except (rnrs) define-record-type error)
    (rnrs r5rs)
    (rnrs mutable-pairs)
    (only (srfi :1 lists) lset-union append-map fold)
    (srfi :6 basic-string-ports)
    (srfi :9 records)
    (srfi :23 error)
    (srfi private include)
    (only (xitomatl common) gensym))

  ;; Create exports for auxiliary keyword so that they work in the Chez repl.
  (define-syntax define-auxiliary-keyword
    (lambda (x)
      (syntax-case x ()
        ((_ keyword)
         #'(define-syntax keyword
             (lambda (x)
               (syntax-violation #f "incorrect usage of auxiliary keyword" x)))))))

  (define-auxiliary-keyword <-)
  (define-auxiliary-keyword !)
  (define-auxiliary-keyword ^)
  ;;(define-auxiliary-keyword /)

  (include/resolve ("packrat") "portable-packrat.scm"))
