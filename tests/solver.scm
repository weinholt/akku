#!/usr/bin/env scheme-script
;;; solver.scm --- Dependency solver unit tests

;; Copyright (C) 2009, 2010 Andreas Rottmann <a.rottmann@gmx.at>
;; Copyright (C) 2009 Daniel Burrows

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is based on the test cases for aptitude's resolver which
;; are written in a DSL and exectuted by the test.cc driver. This
;; being Lisp, there's no need to seperate test specifications and the
;; driver.

;;; Code:

(import (rnrs)
        (akku lib solver)
        (akku lib solver dummy-db)
        (akku lib solver universe)
        (akku lib solver logging)
        (spells match)
        (wak fmt)
        ;; (wak trc-testing)
        (spells logging))

;; Set this to `#t' to show the universe that the solver will operate
;; in.
(define show-universe? #t)

;; Set this to `#t' to get detailed traces of the solver's
;; operation. This is useful for debugging in general, and comparing
;; with aptitude's resolvers' debugging output specifically.
(define debug-output? #t)

(define (simple-log-formatter entry)
  (let ((port (current-output-port))
        (obj (log-entry-object entry)))
    (if (procedure? obj)
        (obj port)
        (display obj port))
    (newline port)))

(define (make-test-db packages dependencies)
  (let ((db (make-dummy-db)))
    (for-each (lambda (package)
                (apply dummy-db-add-package! db package))
              packages)
    (for-each (match-lambda
               ((source version relation . targets)
                (dummy-db-add-dependency! db source version (eq? relation '<>) targets)))
              dependencies)
    db))

(define (test-db->universe db)
  (let ((universe (dummy-db->universe db)))
    (when show-universe?
      (fmt #t (dsp-universe universe)))
    universe))

(define (make-test-universe packages dependencies)
  (test-db->universe (make-test-db packages dependencies)))

(define (make-joint-scores db versions.score-list)
  (map (lambda (versions.score)
         (cons
          (map (lambda (pkg.version)
                 (dummy-db-version-ref db (car pkg.version) (cdr pkg.version)))
               (car versions.score))
          (cdr versions.score)))
       versions.score-list))

(define (make-version-scores db version.score-list)
  (map (lambda (version.score)
         (cons (dummy-db-version-ref db (caar version.score) (cdar version.score))
               (cdr version.score)))
       version.score-list))

(define (test-solutions expected solver)
  (for-each (match-lambda
             ((max-steps 'any)
              (assert (solution? (find-next-solution! solver max-steps))))
             ((max-steps #f)
              (assert (not (find-next-solution! solver max-steps)))))
            expected))

;; (define-test-suite solver-tests
;;   "Dependency resolver")

(define (test1) 
  (let ((universe (make-test-universe
                   '((p1 (1 2 3) 1)
                     (p2 (1 2 3) 1)
                     (p3 (1 2 3) 1)
                     (p4 (1 2 3) 1))
                   '((p1 1 -> (p2 . 2) (p2 . 3))
                     (p1 2 -> (p2 . 2) (p2 . 3))
                     (p1 3 -> (p2 . 2) (p2 . 3))
                     
                     (p2 1 -> (p3 . 1) (p3 . 2) (p3 . 3))
                     (p2 2 -> (p3 . 1) (p3 . 2) (p3 . 3))
                     (p2 1 <> (p1 . 2) (p1 . 3))
                     
                     (p3 1 -> (p4 . 1) (p4 . 2) (p4 . 3))
                     (p3 2 -> (p4 . 1) (p4 . 2) (p4 . 3))
                     (p3 3 -> (p4 . 1) (p4 . 2) (p4 . 3))))))
    (test-solutions '((10000 any)
                      (10000 any)
                      (10000 #f))
      (make-solver universe))))

(define (carglass) 
  (let* ((db (make-test-db
              '((car (1) 1)
                (engine (1 2 #f) #f)
                (turbo (1 #f) 1)
                (wheel (2 3 #f) #f)
                (tyre (1 2 #f) #f)
                (door (1 2 #f) #f)
                (window (0 1 2 #f) #f)
                (glass (1 2 #f) #f))
              '((car 1 -> (engine . 1) (engine . 2))
                (car 1 -> (wheel . 2) (wheel . 3))
                (car 1 -> (door . 1) (door . 2))
                (wheel 3 -> (tyre . 1) (tyre . 2))
                (door 2 -> (window . 0) (window . 1) (window . 2))
                (window 1 -> (glass . 1))
                (window 2 -> (glass . 2))
                (tyre 2 -> (glass . 1) (glass . #f)))))
         (version-scores (make-version-scores db '(((engine . 2) . 100)
                                                   ((wheel . 3) . 100)
                                                   ((tyre . 2) . 100)
                                                   ((door . 2) . 100)
                                                   ((window . 2) . 100)
                                                   ((glass . 2) . 100)))))
    (test-solutions '((10000 any)
                      (10000 any)
                      (10000 any))
      (make-solver (test-db->universe db)
                   `((version-scores . ,version-scores))))))

(define (test3)
  (let ((db (make-test-db '((p1 (1 2 3) 1)
                            (p2 (1 2 3) 1)
                            (p3 (1 2 3) 1)
                            (p4 (1 2 3) 1))
                          '((p1 1 -> (p2 . 2) (p2 . 3))
                            (p1 2 -> (p2 . 2) (p2 . 3))
                            (p1 3 -> (p2 . 2) (p2 . 3))
                            
                            (p2 1 -> (p3 . 1) (p3 . 2) (p3 . 3))
                            (p2 2 -> (p3 . 2) (p3 . 3))
                            (p2 1 <> (p1 . 2) (p1 . 3))
                            
                            (p3 1 -> (p4 . 1) (p4 . 2) (p4 . 3))
                            (p3 2 -> (p4 . 1) (p4 . 2) (p4 . 3))
                            (p3 3 -> (p4 . 1) (p4 . 2) (p4 . 3))))))
    (test-solutions '((10000 any)
                      (10000 any)
                      (10000 any)
                      (10000 #f))
      (make-solver (test-db->universe db)
                   `((version-scores
                      . ,(make-version-scores db '(((p2 . 3) . 100))))
                     (joint-scores
                      . ,(make-joint-scores
                          db
                          `((((p2 . 2) (p3 . 2)) . 500)
                            (((p2 . 2) (p3 . 3)) . 10000)))))))))

(define (test-akku)
  (let ((db (make-test-db
             '((akku (0) 0)
               (chez-srfi (#f 0) #f)
               (compression (#f 0 1) #f)
               (hashing (#f 0 1 2) #f)
               (industria (#f 0 1) #f)
               (ip-address (#f 0) #f)
               (packrat (#f 0) #f)
               (semver (#f 0 1) #f)
               (spdx (#f 0) #f)
               (spells (#f 0) #f)
               (struct-pack (#f 0 1) #f)
               (wak-common (#f 0) #f)
               (wak-fmt (#f 0) #f)
               (wak-foof-loop (#f 0) #f)
               (wak-irregex (#f 0) #f)
               (wak-parscheme (#f 0) #f)
               (wak-riastreams (#f 0) #f)
               (wak-syn-param (#f 0) #f)
               (wak-wt-tree (#f 0) #f)
               (xitomatl (#f 0) #f))

             '((wak-foof-loop 0 -> (chez-srfi . 0))
               (wak-foof-loop 0 -> (wak-common . 0))
               (wak-foof-loop 0 -> (wak-syn-param . 0))
               (wak-foof-loop 0 -> (wak-riastreams . 0))
               (wak-fmt 0 -> (chez-srfi . 0))
               (wak-fmt 0 -> (wak-common . 0))
               (wak-parscheme 0 -> (chez-srfi . 0))
               (wak-parscheme 0 -> (wak-common . 0))
               (wak-parscheme 0 -> (wak-riastreams . 0))
               (packrat 0 -> (chez-srfi . 0))
               (packrat 0 -> (xitomatl . 0))
               (spdx 0 -> (chez-srfi . 0))
               (spdx 0 -> (packrat . 0))
               (xitomatl 0 -> (chez-srfi . 0))
               (wak-irregex 0 -> (wak-common . 0))
               (spells 0 -> (chez-srfi . 0))
               (spells 0 -> (wak-irregex . 0))
               (spells 0 -> (wak-foof-loop . 0))
               (spells 0 -> (wak-fmt . 0))
               (wak-syn-param 0 -> (wak-common . 0))
               (industria 0 -> (chez-srfi . 0))
               (industria 1 -> (chez-srfi . 0))
               (industria 1 -> (hashing . 0) (hashing . 1) (hashing . 2))
               (industria 1 -> (ip-address . 0))
               (industria 1 -> (struct-pack . 0) (struct-pack . 1))
               (semver 0 -> (chez-srfi . 0))
               (semver 0 -> (packrat . 0))
               (semver 0 -> (xitomatl . 0))
               (semver 1 -> (chez-srfi . 0))
               (semver 1 -> (packrat . 0))
               (semver 1 -> (xitomatl . 0))
               (wak-riastreams 0 -> (chez-srfi . 0))
               (wak-riastreams 0 -> (wak-common . 0))
               (akku 0 -> (chez-srfi . 0))
               (akku 0 -> (industria . 1))
               (akku 0 -> (spells . 0))
               (akku 0 -> (semver . 0) (semver . 1))
               (akku 0 -> (spdx . 0))
               (akku 0 -> (wak-fmt . 0))
               (akku 0 -> (wak-riastreams . 0))
               (akku 0 -> (wak-foof-loop . 0))
               (akku 0 -> (wak-wt-tree . 0))
               (akku 0 -> (xitomatl . 0))
               (akku 0 -> (hashing . 1) (hashing . 2))
               (akku 0 -> (compression . 0) (compression . 1))
               (compression 0 -> (chez-srfi . 0))
               (compression 0 -> (hashing . 2))
               (compression 0 -> (struct-pack . 0) (struct-pack . 1))
               (compression 1 -> (chez-srfi . 0))
               (compression 1 -> (hashing . 2))
               (compression 1 -> (struct-pack . 0) (struct-pack . 1))))))
    (test-solutions '((10000 any)
                       ;; (10000 any)
                       ;; (10000 any)
                       ;; (10000 any)
                       ;;  (10000 #f)
                      )
      (make-solver (test-db->universe db)
                   `((version-scores
                      . ,(make-version-scores db '(((akku . 0) . 10000)))))))))


(when debug-output?
  (set-logger-properties! root-logger
                          `((threshold trace)
                            (handlers ,simple-log-formatter))))

;; (test1)
;; (carglass)
;; (test3)
(test-akku)

;; Local Variables:
;; scheme-indent-styles: (trc-testing as-match (test-solutions 1))
;; End:
