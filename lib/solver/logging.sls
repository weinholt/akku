#!r6rs
;;; logging.sls --- Dependency solver, logging utilities

;; Copyright (C) 2009, 2010, 2011, 2015 Andreas Rottmann <a.rottmann@gmx.at>

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

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;;; Code:

(library (akku lib solver logging)
  (export logger:dorodango.solver
          log/trace
          log/debug
          log/info

          internal-error

          dsp-universe
          dsp-package
          dsp-version
          dsp-dependency
          dsp-tier

          dsp-choice
          dsp-choice-set
          dsp-choice-table)
  (import (rnrs)
          (wak fmt)
          (wak foof-loop)
          (wak riastreams)
          (spells alist)
          (spells logging)
          (wak wt-tree)
          (akku private logging)
          (akku lib solver universe)
          (akku lib solver choice))

(define logger:dorodango.solver (make-logger logger:akku 'solver))
(define log/info (make-fmt-log logger:dorodango.solver 'info))
(define log/debug (make-fmt-log logger:dorodango.solver 'debug))
(define log/trace (make-fmt-log logger:dorodango.solver 'trace))

(define (internal-error . formats)
  (assertion-violation 'dorodango.solver
                       (fmt #f (cat "Internal error: " (apply-cat formats)))))

(define (fmt-stream/suffix formatter stream sep)
  (let ((sep (dsp sep)))
    (lambda (st)
      (loop ((for item (in-stream stream))
             (with st st (sep ((formatter item) st))))
        => st))))

(define (dsp-universe universe)
  (cat
   (fmt-stream/suffix dsp-package (universe-package-stream universe) "\n")
   "\n"
   (fmt-stream/suffix dsp-dependency (universe-dependency-stream universe) "\n")))

(define (dsp-package package)
  (cat "package " (package-name package)
       " <" (fmt-join dsp (map version-tag (package-versions package)) " ")
       ">"))

(define (dsp-dependency dependency)
  (cat (dsp-version (dependency-source dependency))
       " -> {" (fmt-join dsp-version (dependency-targets dependency) " ") "}"))

(define (dsp-version version)
  (cat (package-name (version-package version)) " v" (version-tag version)))

(define dsp-tier
  (let ((tier-policy-names
         (map (lambda (pair)
                (cons (tier-policy (car pair)) (cdr pair)))
              `((,maximum-tier . maximum)
                (,conflict-tier . conflict)
                (,already-generated-tier . already-generated)
                (,defer-tier . defer-tier)
                (,minimum-tier . minimum)))))
    (lambda (tier)
      (let* ((priority (tier-priority tier))
             (priority-name (if (= priority (least-fixnum))
                                'least
                                priority)))
        (cond ((assv-ref tier-policy-names (tier-policy tier))
               => (lambda (policy-name)
                    (dsp (cons policy-name priority-name))))
              (else
               (dsp (cons (tier-policy tier) priority-name))))))))

(define (dsp-choice choice)
  (lambda (st)
    ((cat "Install(" (dsp-version (choice-version choice))
          (cond ((choice-dep choice)
                 => (lambda (dep)
                      (cat " "
                           (if (choice-from-dep-source? choice)
                               "<source: "
                               "<")
                           (dsp-dependency dep)
                           ">")))
                (else
                 fmt-null))
          ")") st)))

(define (dsp-choice-set choices)
  (lambda (st)
    ((cat "{" (fmt-join dsp-choice (wt-tree-datums->list choices) ", ") "}") st)))

(define (dsp-choice-table table datum-formatter)
  (define (do-dsp st)
    (cdr (choice-table-fold
                (lambda (choice datum state)
                  (let ((first? (car state))
                        (st (cdr state)))
                    (cons #f
                          ((cat (if first? fmt-null " ")
                                (dsp-choice choice) " -> " (datum-formatter datum)) st))))
                (cons #t st)
                table)))
  (cat "{" do-dsp "}"))

(define (wt-tree-datums->list wt-tree)
  (wt-tree/fold (lambda (key datum lst)
                  (cons datum lst))
                '()
                wt-tree))


)
