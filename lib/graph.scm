;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2017-2019 Göran Weinholt <goran@weinholt.se>
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

;; Print a gv file (for graphviz) showing dependencies.

(library (akku lib graph)
  (export
    print-gv-file)
  (import
    (rnrs (6))
    (chibi match)
    (akku lib file-parser)
    (akku lib repo-scanner)
    (akku lib schemedb))

;;; Generic graphviz code

(define-record-type graph
  (fields name strict? directed? (mutable node*) (mutable subgraph*) (mutable attr*))
  (protocol
   (lambda (p)
     (lambda (name strict? directed?)
       (p name strict? directed? '() '() '())))))

(define (graph-add-node! g name . attr*)
  (let ((node (make-node name)))
    (node-attr*-set! node attr*)
    (graph-node*-set! g (cons node (graph-node* g)))
    node))

(define (graph-add-subgraph! g name)
  (let ((sg (make-graph name (graph-strict? g) (graph-directed? g))))
    (graph-subgraph*-set! g (cons sg (graph-subgraph* g)))
    sg))

(define-record-type node
  (sealed #t)
  (fields name (mutable edge*) (mutable attr*))
  (protocol
   (lambda (p)
     (lambda (name)
       (p name '() '())))))

(define (node-attr-set! node attr value)
  (node-attr*-set! node (cons (cons attr value) (node-attr* node))))

(define-record-type edge
  (sealed #t)
  (fields node (mutable attr*)))

(define (node-connect! v w . attr*)
  (let ((edge (make-edge w attr*)))
    (node-edge*-set! v (cons edge (node-edge* v)))
    edge))

(define (c-string x)
  ;; FIXME: Not exactly right, should be a C-style string.
  (call-with-string-output-port
    (lambda (p)
      (if (string? x)
          (write x p)
          (write (call-with-string-output-port
                   (lambda (p) (display x p)))
                 p)))))

(define (print-graph g p)
  (define (put datum) (display datum p))
  (define (nl) (newline p))
  (define (display-graph g level)
    (define indent
      (case-lambda
        (() (indent level))
        ((level)
         (do ((i 0 (+ i 1)))
             ((= i level))
           (put-char p #\space)))))
    (define (display-node n)
      (indent (+ level 1))
      (put (c-string (node-name n)))
      (display-attr* (node-attr* n))
      (put ";")
      (nl)
      (for-each (lambda (m) (display-edge n m)) (node-edge* n)))
    (define (display-edge n e)
      (indent (+ level 1))
      (let ((m (edge-node e)))
        (put (c-string (node-name n)))
        (if (graph-directed? g)
            (put " -> ")
            (put " -- "))
        (put (c-string (node-name m)))
        (display-attr* (edge-attr* e))
        (put ";")
        (nl)))
    (define (display-attr* attr*)
      (unless (null? attr*)
        (put " [")
        (let lp ((attr* attr*))
          (unless (null? attr*)
            (put (caar attr*))
            (put "=")
            (put (c-string (cdar attr*)))
            (unless (null? (cdr attr*))
              (put #\,)
              (lp (cdr attr*)))))
        (put "]")))
    (indent)
    (cond ((zero? level)
           (when (graph-strict? g)
             (put "strict "))
           (when (graph-directed? g)
             (put "di")))
          (else
           (put "sub")))
    (put "graph ")
    (put (c-string (graph-name g)))
    (put " {")
    (nl)
    (for-each (lambda (sg) (display-graph sg (+ level 1))) (graph-subgraph* g))
    (for-each (lambda (n) (display-node n)) (graph-node* g))
    (for-each (lambda (attr) #f) (graph-attr* g))
    (indent)
    (put "}")
    (nl))

  (display-graph g 0))

;;; Analysis

(define (artifacts->graph artifact* exclude-tests?)
  (let* ((G (make-graph "G" #f #t))
         (Gp (graph-add-subgraph! G "Project")))
    (for-each
     (lambda (artifact)
       (let ((node
              (cond
                ((and exclude-tests? (artifact-for-test? artifact))
                 #f)
                ((r6rs-library? artifact)
                 (graph-add-node! Gp (r6rs-library-name artifact)))
                ((r6rs-program? artifact)
                 (graph-add-node! Gp (artifact-path artifact) '(shape . star)
                                  `(fillcolor . ,(if (artifact-for-test? artifact)
                                                     'grey 'yellow))))
                (else #f))))
         (when node
           (node-attr-set! node 'style 'filled)
           (when (artifact-for-test? artifact)
             (node-attr-set! node 'fillcolor 'blue))
           (for-each
            (lambda (lib-ref)
              (call/cc
                (lambda (k)
                  (for-each
                   (lambda (artifact^)
                     (when (and (library-reference-satisfied? lib-ref artifact^)
                                (r6rs-library-name artifact^))
                       ;; XXX: find the node for the library
                       (node-connect! node (graph-add-node! Gp (r6rs-library-name artifact^)))
                       (k)))
                   artifact*))))
            (artifact-imports artifact))
           (for-each
            (lambda (asset)
              (node-connect! node (graph-add-node! Gp (include-reference-path asset)
                                                   '(fillcolor . gold)
                                                   '(shape . tab)
                                                   '(style . filled))))
            (artifact-assets artifact)))))
     artifact*)
    G))

(define (print-gv-file base-directory)
  (let* ((artifact* (find-artifacts base-directory (scm-file-list base-directory)))
         (G (artifacts->graph artifact* #t)))
    (print-graph G (current-output-port)))))
