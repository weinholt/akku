;;; -*- Mode: Scheme -*-

;;;; Expanding Vectors

;;; Copyright (c) 2009, Taylor R. Campbell
;;;
;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:
;;;
;;; * Redistributions of source code must retain the above copyright
;;;   notice, this list of conditions and the following disclaimer.
;;;
;;; * Redistributions in binary form must reproduce the above copyright
;;;   notice, this list of conditions and the following disclaimer in
;;;   the documentation and/or other materials provided with the
;;;   distribution.
;;;
;;; * Neither the names of the authors nor the names of contributors
;;;   may be used to endorse or promote products derived from this
;;;   software without specific prior written permission.
;;;
;;; THIS SOFTWARE IS PROVIDED BY THE AUTHORS ``AS IS'' AND ANY EXPRESS
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; This implements singly resizable arrays, as described in
;;;
;;;   Andrej Brodnik, Svante Carlsson, Robert Sedgwick, J. Ian Munro,
;;;   and Erik D. Demaine, `Resizable Arrays in Optimal Time and
;;;   Space', Department of Computer Science, University of Waterloo.
;;;   Technical Report CS-99-09, 1999.
;;;
;;; This code is unfinished and only lightly tested.  Please do not
;;; use it in real code.  The exported API is incomplete and there are
;;; undoubtedly bugs still lurking in this code.

(define-record-type <xvector>
    (%make-xvector
     length                      ;Number of elements in the xvector
     super-block                 ;Number of largest super block
     data-block-count            ;Number of allocated data blocks total
     super-block-size            ;Number of blocks total in this super block
     super-block-free            ;Number of blocks free in this super block
     data-blocks                 ;Vector of data blocks
     )
    xvector?
  (length xvector.length set-xvector.length!)
  (super-block xvector.super-block set-xvector.super-block!)
  (data-block-count xvector.data-block-count set-xvector.data-block-count!)
  (super-block-size xvector.super-block-size set-xvector.super-block-size!)
  (super-block-free xvector.super-block-free set-xvector.super-block-free!)
  (data-blocks xvector.data-blocks set-xvector.data-blocks!))

(define (make-xvector)
  (%make-xvector 0 0 1 1 0 (vector (make-data-block 0 0 (make-vector 1)))))

(define (xvector-length xvector)
  (if (not (xvector? xvector))
      (error "Not an xvector:" xvector `(XVECTOR-LENGTH ,xvector)))
  (xvector.length xvector))

(define-record-type <data-block>
    (make-data-block super-block element-count elements)
    data-block?
  (super-block data-block.super-block)
  (element-count data-block.element-count set-data-block.element-count!)
  (elements data-block.elements))

(define (xvector-locate index)
  (let* ((r (+ index 1))
         (k (- (integer-length r) 1)))
    (let ((floor-k/2 (arithmetic-shift k -1))
          (ceiling-k/2 (arithmetic-shift (+ k 1) -1)))
      ;; The paper gives the incorrect formula 2^k - 1 for the hairy
      ;; expression of p, which is the number of data blocks preceding
      ;; the kth superblock.  That number is computed by
      ;;   2^0 + 2^0 + 2^1 + 2^1 + ...
      ;;     + 2^((floor (k / 2)) - 1) + 2^((floor (k / 2)) - 1),
      ;; with an extra addend of 2^(floor (k / 2)) if floor (k / 2) is
      ;; odd.  Write that sum as twice a geometric series, and then
      ;; simplify, and you'll get this.
      (let ((p
             (+ (arithmetic-shift (bit-mask floor-k/2) 1)
                (* (arithmetic-shift 1 floor-k/2) (bitwise-and k 1))))
            (b (extract-bit-field floor-k/2 ceiling-k/2 r))
            (e (extract-bit-field ceiling-k/2 0 r)))
        (values (+ p b) e)))))

(define (xvector-locate-vector xvector index)
  (receive (block-index element-index) (xvector-locate index)
    (values
     (data-block.elements
      (vector-ref (xvector.data-blocks xvector) block-index))
     element-index)))

(define (xvector-ref xvector index)
  (if (not (and (xvector? xvector)
                (integer? index)
                (exact? index)
                (<= 0 index)
                (< index (xvector.length xvector))))
      (xvector-ref:lose xvector index))
  (receive (vector index) (xvector-locate-vector xvector index)
    (vector-ref vector index)))

(define (xvector-set! xvector index element)
  (if (not (and (xvector? xvector)
                (integer? index)
                (exact? index)
                (<= 0 index)
                (< index (xvector.length xvector))))
      (xvector-set!:lose xvector index element))
  (receive (vector index) (xvector-locate-vector xvector index)
    (vector-set! vector index element)))

(define (xvector-ref:lose xvector index)
  (xvector-lose xvector index `(XVECTOR-REF ,xvector ,index)))

(define (xvector-set!:lose xvector index element)
  (xvector-lose xvector index `(XVECTOR-SET! ,xvector ,index ,element)))

(define (xvector-lose xvector index caller)
  (if (not (xvector? xvector))
      (error "Not an xvector:" xvector caller))
  (if (not (and (integer? index)
                (exact? index)
                (< 0 index)))
      (error "Not an xvector index:" index caller))
  (if (not (< index (xvector.length xvector)))
      (error "Xvector index out of bounds:" index caller)))

(define (xvector-push! xvector element)
  (let ((data-block-count (xvector.data-block-count xvector))
        (data-blocks (xvector.data-blocks xvector)))
    (let ((data-block (vector-ref data-blocks (- data-block-count 1))))
      (let ((elements (data-block.elements data-block))
            (element-count (data-block.element-count data-block)))
        (if (< element-count (vector-length elements))
            (begin
              (vector-set! elements element-count element)
              (set-data-block.element-count! data-block (+ element-count 1)))
            (let ((super-block-free (xvector.super-block-free xvector)))
              (if (positive? super-block-free)
                  (let ((super-block-free* (- super-block-free 1)))
                    (set-xvector.super-block-free! xvector super-block-free*)
                    (allocate-data-block xvector element-count element))
                  (let ((element-count*
                         (allocate-super-block xvector element-count)))
                    (allocate-data-block xvector element-count* element))))))))
  (set-xvector.length! xvector (+ 1 (xvector.length xvector))))

(define (allocate-data-block xvector data-block-size element)
  (let ((data-block-count (xvector.data-block-count xvector)))
    (let* ((elements (make-vector data-block-size))
           (data-block
            (make-data-block (xvector.super-block xvector) 1 elements)))
      (vector-set! elements 0 element)
      (vector-set! (let ((data-blocks (xvector.data-blocks xvector)))
                     (if (< data-block-count (vector-length data-blocks))
                         data-blocks
                         (let ((data-blocks
                                (vector-grow!
                                 data-blocks
                                 (arithmetic-shift data-block-count 1))))
                           (set-xvector.data-blocks! xvector data-blocks)
                           data-blocks)))
                   data-block-count
                   data-block))
    (set-xvector.data-block-count! xvector (+ 1 data-block-count))))

;;; This returns not the number of the newly allocated super block, but
;;; rather the number of elements in any data block in that super
;;; block, since that number is what the sole caller in XVECTOR-PUSH!
;;; needs.

(define (allocate-super-block xvector data-block-size)
  (let ((super-block* (+ 1 (xvector.super-block xvector)))
        (super-block-size (xvector.super-block-size xvector)))
    (receive (super-block-size* data-block-size*)
             (if (odd? super-block*)
                 (values super-block-size
                         (arithmetic-shift data-block-size 1))
                 (let ((super-block-size*
                        (arithmetic-shift super-block-size 1)))
                   (set-xvector.super-block-size! xvector super-block-size*)
                   (values super-block-size* data-block-size)))
      (set-xvector.super-block! xvector super-block*)
      (set-xvector.super-block-free! xvector (- super-block-size* 1))
      data-block-size*)))

(define (xvector-pop! xvector)
  (set-xvector.length! xvector (- (xvector.length xvector) 1))
  (let ((data-block-count (xvector.data-block-count xvector))
        (data-blocks (xvector.data-blocks xvector)))
    (let ((data-block (vector-ref data-blocks (- data-block-count 1))))
      (let ((element-count (data-block.element-count data-block))
            (elements (data-block.elements data-block)))
        (let* ((element-count* (- element-count 1))
               (element (vector-ref elements element-count*)))
          (if (or (> element-count* 0) (= data-block-count 1))
              (begin
                (set-data-block.element-count! data-block element-count*)
                (vector-set! elements element-count* (uninitialized)))
              (begin
                (deallocate-data-block xvector)
                (let ((super-block-free*
                       (+ 1 (xvector.super-block-free xvector))))
                  (if (= super-block-free* (xvector.super-block-size xvector))
                      (deallocate-super-block xvector)
                      (set-xvector.super-block-free! xvector
                                                     super-block-free*)))))
          element)))))

(define (deallocate-super-block xvector)
  (set-xvector.super-block-free! xvector 0)
  (let ((super-block* (- (xvector.super-block xvector) 1)))
    (if (odd? super-block*)
        (set-xvector.super-block-size!
         xvector
         (arithmetic-shift (xvector.super-block-size xvector) -1)))
    (set-xvector.super-block! xvector super-block*)))

(define (deallocate-data-block xvector)
  (let ((data-block-count (xvector.data-block-count xvector))
        (data-blocks (xvector.data-blocks xvector)))
    (let ((data-block-count* (- data-block-count 1)))
      (set-xvector.data-block-count! xvector data-block-count*)
      (vector-set! data-blocks data-block-count* (uninitialized))
      (let ((size (vector-length data-blocks)))
        (if (<= data-block-count* (arithmetic-shift size -2))
            (set-xvector.data-blocks!
             xvector
             (vector-shrink! data-blocks (arithmetic-shift size -1))))))))

;;; This page contains several equivalent definitions of procedures,
;;; to be compared for performance.

(define (xvector-walk xvector procedure)
  (xvector-walk/direct xvector procedure))

(define (xvector->list procedure)
  (xvector->list/direct procedure))

(define (list->xvector lst)
  (let ((result (make-xvector)))
    (do ((lst lst (cdr lst)))
        ((null? lst) result)
      (xvector-push! result (car lst)))))

(define (xvector-walk/indirect xvector procedure)
  (do ((i 0 (+ i 1)))
      ((>= i (xvector-length xvector)))
    (procedure i (xvector-ref xvector i))))

(define (xvector->list/indirect xvector-walk xvector)
  (let ((list '()))
    (xvector-walk xvector
      (lambda (index element)
        index                           ;ignore
        (set! list (cons element list))))
    (reverse list)))

(define (xvector-walk/direct xvector procedure)
  (let ((data-blocks (xvector.data-blocks xvector))
        (data-block-count (xvector.data-block-count xvector)))
    (let data-block-loop ((data-block-index 0) (element-offset 0))
      (if (< data-block-index data-block-count)
          (let ((data-block (vector-ref data-blocks data-block-index)))
            (let ((elements (data-block.elements data-block))
                  (element-count (data-block.element-count data-block)))
              (let element-loop ((element-index 0))
                (if (< element-index element-count)
                    (begin
                      (procedure (+ element-index element-offset)
                                 (vector-ref elements element-index))
                      (element-loop (+ element-index 1)))))
              (data-block-loop (+ data-block-index 1)
                               (+ element-offset element-count))))))))

(define (xvector->list/direct xvector)
  (let ((data-blocks (xvector.data-blocks xvector))
        (data-block-count (xvector.data-block-count xvector)))
    (let loop ((data-block-count data-block-count) (list '()))
      (if (zero? data-block-count)
          list
          (let ((data-block-index (- data-block-count 1)))
            (loop data-block-index
                  (let ((data-block (vector-ref data-blocks data-block-index)))
                    (let ((elements (data-block.elements data-block))
                          (element-count
                           (data-block.element-count data-block)))
                      (let loop ((element-count element-count) (list list))
                        (if (zero? element-count)
                            list
                            (let ((element-index (- element-count 1)))
                              (loop element-index
                                    (cons (vector-ref elements element-index)
                                          list)))))))))))))

;;; On this page you will find procedures that can be tuned to your
;;; particular Scheme system.  For example, if you can shrink a vector
;;; destructively, letting the garbage collector reclaim the unused
;;; memory when it next runs, you should do that, rather than copying.
;;; Also, if your Scheme system provides a native INTEGER-LENGTH,
;;; please use that, rather than the commented definition below!

(define (vector-grow! vector size)
  (let ((vector* (make-vector size)))
    (do ((i 0 (+ i 1)))
        ((>= i (vector-length vector)))
      (vector-set! vector* i (vector-ref vector i)))
    vector*))

(define (vector-shrink! vector size)
  ;; (set-vector-length! vector size)
  (let ((vector* (make-vector size)))
    (do ((i 0 (+ i 1)))
        ((>= i size))
      (vector-set! vector* i (vector-ref vector i)))
    vector*))

(define (uninitialized)
  (if #f #f))

(define (bit-mask size)
  (bitwise-not (arithmetic-shift -1 size)))

;;; SRFI 33 definitions:

;(define (extract-bit-field size position integer)
;  (bitwise-and (bit-mask size) (arithmetic-shift integer (- 0 position))))
;
;(define (integer-length integer)
;  (let loop ((integer integer) (length 0))
;    (if (zero? integer)
;        length
;        (loop (arithmetic-shift integer -1)
;              (+ length 1)))))
