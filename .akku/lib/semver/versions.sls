#!r6rs ;; -*- mode: scheme; coding: utf-8 -*-
;; Copyright © 2018 Göran Weinholt <goran@weinholt.se>
;; SPDX-License-Identifier: MIT
#!r6rs

(library (semver versions)
  (export
    make-semver semver?
    semver-major semver-minor semver-patch semver-pre-release-ids semver-build-ids

    string->semver
    semver->string
    semver-compare
    semver-increment-major
    semver-increment-minor
    semver-increment-patch)
  (import
    (rnrs)
    (srfi :67 compare-procedures)
    (srfi :115 regexp))

;;; Construction

(define rx-semver
  (rx (-> major (or "0" (: (/ "19") (* digit)))) "."
      (-> minor (or "0" (: (/ "19") (* digit)))) "."
      (-> patch (or "0" (: (/ "19") (* digit))))
      (optional "-" (-> pre-release (+ (~ "+"))))
      (optional "+" (-> build-id (+ any)))))

(define rx-pre-release-invalid-numeric-id
  (rx "0" (+ (/ "09"))))

(define rx-pre-release-numeric-id
  (rx (or "0" (: (/ "19") (* (/ "09"))))))

(define rx-pre-release-id
  (rx (+ (or (/ "09AZaz") "-"))))

(define rx-build-id
  (rx (+ (or (/ "09AZaz") "-"))))

(define rx-dot
  (rx "."))

(define-record-type semver
  (nongenerative semver-v0-b75bb3c1-766c-4845-bb45-84824475d95d)
  (sealed #t)
  (fields major                       ;exact non-negative integers
          minor
          patch
          pre-release-ids             ;list of strings or integers
          build-ids)                  ;list of strings
  (protocol
   (lambda (p)
     (case-lambda
       ((major minor patch)
        (validate (p major minor patch '() '())))
       ((major minor patch pre-release)
        (validate (p major minor patch (map normalize-pre-release-id pre-release) '())))
       ((major minor patch pre-release build)
        (validate (p major minor patch (map normalize-pre-release-id pre-release) build)))))))

(define (normalize-pre-release-id id)
  (or (and (string? id)
           (regexp-matches rx-pre-release-numeric-id id)
           (string->number id 10))
      id))

(define (validate semver)
  (define (validate-number n)
    (unless (and (number? n) (exact? n) (integer? n) (not (negative? n)))
      (assertion-violation 'make-semver "Invalid semver" (semver->string semver) n)))
  (define (validate-pre-release-id id)
    (unless (or (and (number? id) (exact? id) (integer? id) (not (negative? id)))
                (and (string? id)
                     (not (regexp-matches rx-pre-release-invalid-numeric-id id))
                     (regexp-matches rx-pre-release-id id)))
      (assertion-violation 'make-semver "Invalid pre-release" (semver->string semver) id)))
  (define (validate-build-id id)
    (unless (and (string? id) (regexp-matches rx-build-id id))
      (assertion-violation 'make-semver "Invalid build id" (semver->string semver) id)))
  (validate-number (semver-major semver))
  (validate-number (semver-minor semver))
  (validate-number (semver-patch semver))
  (for-each validate-pre-release-id (semver-pre-release-ids semver))
  (for-each validate-build-id (semver-build-ids semver))
  semver)

(define (string->semver str)
  (let ((m (regexp-matches rx-semver str)))
    (unless m
      (error 'string->semver "Invalid semver" str))
    (let ((major (string->number (regexp-match-submatch m 1) 10))
          (minor (string->number (regexp-match-submatch m 2) 10))
          (patch (string->number (regexp-match-submatch m 3) 10))
          (pre-release (regexp-match-submatch m 4))
          (build (regexp-match-submatch m 5)))
      (make-semver major minor patch
                   (if pre-release (regexp-split rx-dot pre-release) '())
                   (if build (regexp-split rx-dot build) '())))))

(define (semver->string semver)
  (define (put-id-list p ids)
    (display (car ids) p)
    (unless (null? (cdr ids))
      (put-char p #\.)
      (put-id-list p (cdr ids))))
  (call-with-string-output-port
    (lambda (p)
      (put-datum p (semver-major semver))
      (put-char p #\.)
      (put-datum p (semver-minor semver))
      (put-char p #\.)
      (put-datum p (semver-patch semver))
      (when (pair? (semver-pre-release-ids semver))
        (put-char p #\-)
        (put-id-list p (semver-pre-release-ids semver)))
      (when (pair? (semver-build-ids semver))
        (put-char p #\+)
        (put-id-list p (semver-build-ids semver))))))

(define (semver-compare s0 s1)
  (if3 (refine-compare
        (number-compare (semver-major s0) (semver-major s1))
        (number-compare (semver-minor s0) (semver-minor s1))
        (number-compare (semver-patch s0) (semver-patch s1)))
       -1
       (let ((ids0 (semver-pre-release-ids s0))
             (ids1 (semver-pre-release-ids s1)))
         (if (and (pair? ids0) (null? ids1))
             -1
             (if (and (pair? ids1) (null? ids0))
                 1
                 (list-compare (lambda (id0 id1)
                                 (if (string? id0)
                                     (if (string? id1)
                                         (string-compare id0 id1)
                                         1)
                                     (if (string? id1)
                                         -1
                                         (number-compare id0 id1))))
                               ids0 ids1))))
       1))

(define semver-increment-major
  (case-lambda
    ((semver)
     (semver-increment-major semver '() '()))
    ((semver pre-release-ids)
     (semver-increment-major semver pre-release-ids '()))
    ((semver pre-release-ids build-ids)
     (make-semver (+ 1 (semver-major semver))
                  0
                  0
                  pre-release-ids build-ids))))

(define semver-increment-minor
  (case-lambda
    ((semver)
     (semver-increment-minor semver '() '()))
    ((semver pre-release-ids)
     (semver-increment-minor semver pre-release-ids '()))
    ((semver pre-release-ids build-ids)
     (make-semver (semver-major semver)
                  (+ 1 (semver-minor semver))
                  0
                  pre-release-ids build-ids))))

(define semver-increment-patch
  (case-lambda
    ((semver)
     (semver-increment-patch semver '() '()))
    ((semver pre-release-ids)
     (semver-increment-patch semver pre-release-ids '()))
    ((semver pre-release-ids build-ids)
     (make-semver (semver-major semver)
                  (semver-minor semver)
                  (+ 1 (semver-patch semver))
                  pre-release-ids build-ids)))))
