#!r6rs
;; Copyright (C) 2009, 2010, 2011, 2015 Andreas Rottmann <a.rottmann@gmx.at>

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. The name of the author may not be used to endorse or promote products
;;    derived from this software without specific prior written permission.

;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
;; IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
;; OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
;; IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
;; INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
;; NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
;; DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
;; THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
;; (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
;; THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(library (akku private utils)
  (export in-hashtable

          xvector-remove-first!
          xvector-remove
          in-xvector
          fmt-join/xvector

          define-guarantor)
  (import (except (rnrs) file-exists? delete-file)
          (only (srfi :1) drop-right last)
          (srfi :8 receive)
          (srfi :98 os-environment-variables)
          (wak foof-loop)
          (wak fmt)
          (only (spells misc) and=>)
          (spells alist)
          (spells xvector)
          (wak wt-tree))

(define-syntax define-guarantor
  (syntax-rules ()
    ((define-guarantor guarantor predicate type-name)
     (define (guarantor obj who)
       (if (predicate obj)
           obj
           (assertion-violation who
                                (string-append "invalid argument type (expected "
                                               type-name ")")
                                obj))))))

(define-syntax in-hashtable
  (syntax-rules ()
    ((_ (key-var datum-var) (hashtable-expr) cont . env)
     (cont
      (((keys datums size)                       ;Outer bindings
        (let ((hashtable hashtable-expr))
          (receive (keys datums)
                   (hashtable-entries hashtable)
            (values keys datums (vector-length keys))))))
      ((index 0 (+ index 1)))                    ;Loop variables
      ()                                         ;Entry bindings
      ((= index size))                           ;Termination conditions
      (((key-var datum-var)
        (values (vector-ref keys index)
                (vector-ref datums index))))     ;Body bindings
      ()                                         ;Final bindings
      . env))))


;;; xvector utilities

;; Removes (at most) a single item from xvector; does not keep
;; relative order
(define (xvector-remove-first! vec value =?)
  (loop continue ((for i (up-from 0 (to (xvector-length vec)))))
    (cond ((=? value (xvector-ref vec i))
           (let ((last-element (xvector-pop! vec)))
             (unless (= i (xvector-length vec)) ;shall we remove the last?
               (xvector-set! vec i last-element))))
          (else
           (continue)))))

;; Removes all matching values, and returns a new xvector. Keeps
;; relative order.
(define (xvector-remove vec value =?)
  (let ((result (make-xvector)))
    (loop ((for i (up-from 0 (to (xvector-length vec)))))
      => result
      (let ((elt (xvector-ref vec i)))
        (unless (=? elt value)
          (xvector-push! result elt))))))

(define-syntax in-xvector
  (syntax-rules ()
    ((_ (element-var) (xvector-expr) cont . env)
     (cont
      (((xvector size)                       ;Outer bindings
        (let ((xvector xvector-expr))
          (values xvector (xvector-length xvector)))))
      ((index 0 (+ index 1)))                    ;Loop variables
      ()                                         ;Entry bindings
      ((= index size))                           ;Termination conditions
      (((element-var)                            ;Body bindings
        (xvector-ref xvector index)))
      ()                                         ;Final bindings
      . env))))

(define (fmt-join/xvector formatter vec sep)
  (lambda (st)
    (let ((len (xvector-length vec)))
      (loop ((for i (up-from 0 (to len)))
             (with st st
                   ((cat (formatter (xvector-ref vec i))
                         (if (< i (- len 1)) sep fmt-null))
                    st)))
        => st)))))
