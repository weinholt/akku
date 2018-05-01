#!r6rs
;;; solver.sls --- Dependency solver, algorithm

;; Copyright (C) 2009-2011, 2015 Andreas Rottmann <a.rottmann@gmx.at>
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

;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; This file corresponds to aptitude's "problemresolver.h" and
;; "solution.h".

;;; Code:

(library (akku lib solver)
  (export make-solver
          solver?
          find-next-solution!

          solution?
          solution-choices

          logger:dorodango.solver)
  (import (rnrs)
          (srfi :2 and-let*)
          (srfi :8 receive)
          (srfi :67 compare-procedures)
          (spells hash-utils)
          (spells record-types)
          (spells tracing)
          (spells misc)
          (spells alist)
          (spells logging)
          (wak riastreams)
          (wak fmt)
          (wak foof-loop)
          (wak wt-tree)
          (akku private utils)
          (akku lib solver logging)
          (akku lib solver choice)
          (akku lib solver search-graph)
          (akku lib solver promotions)
          (akku lib solver universe)
          (akku lib solver expression))


;;; The solver

(define-record-type* solver
  (really-make-solver find-next)
  ())

(define make-solver
  (case-lambda
    ((universe options)
     (construct-solver universe options))
    ((universe)
     (make-solver universe '()))))

;; The solver constructor -- the solver is obviously an abomination of
;; state, but what do you expect from an algorithm implementation
;; taken from an imperative language?
(define (construct-solver universe options)
  (letrec*
      ((promotion-retracted
        (lambda (promotion)
          (log/debug
           "Retracting all tier assignments that might be linked to "
           (dsp-promotion promotion))))
       
       ;; Options
       (option
        (lambda (name)
          (option-ref options name)))
       
       (score/step (option 'step-score))
       (score/broken (option 'broken-score))
       (score/infinity (option 'infinity))
       (score/goal (option 'goal-score))
       (score/full-solution (option 'full-solution-score))
       
       (future-horizon (option 'future-horizon))
       (remove-stupid? (option 'remove-stupid?))
       (initial-state (extend-installation (make-empty-installation)
                                           (option 'initial-choices)))
       (joint-scores (make-joint-score-set initial-state (option 'joint-scores)))
       (version-scores (make-version-scores universe (option 'version-scores)))
       
       ;; Immutable
       (initial-broken
        (universe-broken-dependency-set universe initial-state))
       (minimum-score (- score/infinity))
       
       ;; Solver state
       (finished? #f)
       (promotions (make-promotion-set universe promotion-retracted))
       (graph (make-search-graph promotions))
       (graph-wt-type (search-graph->wt-tree-type graph))
       (pending (make-wt-tree graph-wt-type))
       (num-deferred 0)
       (minimum-search-tier minimum-tier)
       (maximum-search-tier minimum-tier)
       (closed (make-hashtable step-contents-hash step-contents=?))
       (pending-future-solutions (make-wt-tree graph-wt-type))
       (promotion-queue-tail (make-promotion-queue 0 0))
       (version-tiers (make-vector (universe-version-count universe)
                                   minimum-tier)))

    ;;; Public procedures
    
    (define (find-next max-steps)
      (and (not finished?)
           (begin
             (when (and (wt-tree/empty? pending)
                        (wt-tree/empty? pending-future-solutions)
                        (= 0 (hashtable-size closed)))
               (start-search))
             (loop continue
                 ((for remaining-steps (down-from max-steps (to 0)))
                  (with most-future-solution-steps 0)
                  (while (contains-canditate? pending)))
               => (prepare-result most-future-solution-steps)
               (when (> most-future-solution-steps 0)
                 (log/debug "Speculative \"future\" resolver tick ("
                            most-future-solution-steps "/" future-horizon ")."))
               (let ((cur-step-num (wt-tree/min pending)))
                 (wt-tree/delete! pending cur-step-num)
                 (process-step cur-step-num))
               (let ((new-most-future-solutions-steps
                      (if (contains-canditate? pending-future-solutions)
                          (+ most-future-solution-steps 1)
                          0)))
                 (log/trace "Done generating successors.")
                 (search-graph-run-scheduled-promotion-propagations!
                  graph
                  add-promotion)
                 (continue
                  (=> most-future-solution-steps new-most-future-solutions-steps)))))))

    ;;; Private procedures
    
    (define (prepare-result most-future-solution-steps)
      (log/trace
       (cond ((> most-future-solution-steps future-horizon)
              "Done examining future steps for a better solution.")
             ((not (contains-canditate? pending))
              "Exhausted all search branches.")
             (else
              "Ran out of time.")))
      (cond ((contains-canditate? pending-future-solutions)
             (let* ((best-future-solution (wt-tree/min pending-future-solutions))
                    (best-future-solution-step
                     (search-graph-step graph best-future-solution)))
               (sanity-check-not-deferred best-future-solution)
               (let ((result (make-solution (step-actions best-future-solution-step)
                                            (step-score best-future-solution-step)
                                            (step-tier best-future-solution-step))))
                 (log/debug "--- Returning the future solution "
                            (dsp-solution result) " from step " best-future-solution)
                 (wt-tree/delete! pending-future-solutions best-future-solution)
                 result)))
            ((contains-canditate? pending)
             (raise (condition (make-out-of-time-condition))))
            (else
             (set! finished? #t)
             #f)))
    
    (define (contains-canditate? step-set)
      (and (not (wt-tree/empty? step-set))
           (tier<? (step-tier (search-graph-step graph (wt-tree/min step-set)))
                   defer-tier)))
    
    (define (current-search-tier)
      (if (wt-tree/empty? pending)
          minimum-tier
          (step-tier (search-graph-step graph (wt-tree/min pending)))))
    
    (define (start-search)
      (log/debug "Starting a new search.")
      (let* ((broken-size (wt-tree/size initial-broken))
             (root-score (+ (* broken-size score/broken)
                            (if (= 0 broken-size)
                                score/full-solution
                                0)))
             (root (search-graph-add-root-step! graph root-score)))
        (set-step-promotion-queue-location! root promotion-queue-tail)
        (wt-tree/for-each (lambda (dependency true)
                            (add-unresolved-dep root dependency))
                          initial-broken)
        (log/debug "Inserting the root at step " (step-num root)
                   " with tier " (dsp-tier (step-tier root)))
        (wt-tree/add! pending (step-num root) #t)))

    (define (process-step step-num)
      (loop continue ((with step-num step-num))
        (let ((step (search-graph-step graph step-num)))
          (log/debug "Examining step " step-num " " (dsp-step step))
          (when (tier>=? (step-tier step) defer-tier)
            (internal-error "the tier of step " step-num
                            " is an unprocessed tier, so why is it a candidate?"))
          (check-for-new-promotions step-num)
          (sanity-check-promotions step)
          (cond
            ((already-seen? step-num)
             (log/debug "Dropping already visited search node in step "
                        step-num))
            ((irrelevant? step)
             (log/debug "Dropping irrelevant step " step-num))
            ((tier>=? (step-tier step) defer-tier)
             (log/debug "Skipping newly deferred step " step-num)
             (wt-tree/add! pending step-num #t))
            (else
             (log/trace "Processing step " step-num)
             (hashtable-set! closed step step-num)
             (cond ((step-solution? step)
                    (log/debug " --- Found solution at step " step-num
                               ": " (dsp-step step))
                    (add-promotion step-num
                                   (make-promotion
                                    (generalize-choice-set (step-actions step))
                                    already-generated-tier))
                    (set-step-blessed-solution?! step #t)
                    (wt-tree/add! pending-future-solutions step-num #t))
                   (else
                    (generate-successors step-num)
                    (and-let* ((child-num (step-first-child step))
                               (child (search-graph-step graph child-num))
                               ((step-last-child? child))
                               ((tier<? (step-tier child) defer-tier)))
                      (log/trace "Following forced dependency resolution from step "
                                 step-num " to step " child-num)
                      (wt-tree/delete! pending child-num)
                      (continue (=> step-num child-num))))))))))

    (define (generate-successors step-num)
      (log/trace "Generating successors for step " step-num)
      (let ((parent (search-graph-step graph step-num)))
        (receive (best-dep best-solvers)
                 (step-unresolved-deps-min parent)
          (let ((num-solvers (solver-tracker-size best-solvers)))
            (unless (> num-solvers 0)
              (internal-error "A step containing a dependency with no solvers"
                              " was not promoted to the conflict tier."))
            (log/trace "Generating successors for step " step-num
                       " for the dependency " (dsp-solver-tracker best-solvers)
                       " with " num-solvers " solvers: "
                       (dsp-solver-tracker-solvers best-solvers)))
          (solver-tracker-fold
           (lambda (solver info first?)
             (check-for-new-promotions step-num)
             (unless first?
               (set-step-last-child?! (search-graph-last-step graph) #f))
             (generate-single-successor parent
                                        solver
                                        (max-compare tier-compare
                                                     (solver-info-tier info)
                                                     (step-tier parent)))
             #f)
           #t
           best-solvers))))

    (define (generate-single-successor parent original-choice tier)
      (let* ((reason (choice-with-id original-choice
                                     (choice-set-size (step-actions parent))))
             (step (search-graph-add-step! graph
                                           parent
                                           tier
                                           reason
                                           (build-is-deferred-listener reason))))
        (set-step-promotion-queue-location! step promotion-queue-tail)
        (log/trace "Generating a successor to step " (step-num parent)
                   " for the action " (dsp-choice reason) " with tier "
                   (dsp-tier tier) " and outputting to step " (step-num step))
        (unless (choice-dep reason)
          (internal-error "No dependency attached to the choice "
                          (dsp-choice reason) " used to generate step "
                          (step-num step)))
        (step-add-action! step reason) ; includes steps 1 and 2
        (strike-structurally-forbidden step reason) ; steps 3, 4
        (add-new-unresolved-deps step reason)       ; step 5
        (find-new-incipient-promotions step reason) ; step 6
        (extend-score-to-new-step step reason)
        (log/trace "Generated step " (step-num step)
                   " (" (choice-set-size (step-actions step)) " actions): "
                   (dsp-choice-set (step-actions step)) ";T" (dsp-tier (step-tier step))
                   "S" (step-score step))
        (when (and (tier>=? (step-tier step) defer-tier)
                   (tier<? (step-tier step) already-generated-tier))
          (set! num-deferred (+ num-deferred 1)))
        (wt-tree/add! pending (step-num step) #t)))

    (define (extend-score-to-new-step step choice)
      (step-increase-action-score! step score/step)
      (let* ((version (choice-version choice))
             (old-version (version-of (version-package version) initial-state))
             (new-score (vector-ref version-scores (version-id version)))
             (old-score (vector-ref version-scores (version-id old-version))))
        (log/trace "Modifying the score of step " (step-num step)
                   " by " (num/sign (- new-score old-score))
                   " to account for the replacement of "
                   (dsp-version old-version) " (score " old-score ") by "
                   (dsp-version version) " (score " new-score ")")
        (step-increase-action-score! step (- new-score old-score))
        (joint-score-set-visit
         joint-scores
         choice
         (lambda (choices score)
           (when (choice-set-subset? choices (step-actions step))
             (log/trace "Adjusting the score of " (step-num step)
                        " by " (num/sign score) " for a joint score constraint on "
                        (dsp-choice-set choices))
             (step-increase-action-score! step score)
             #f ;don't stop
             )))
        (let ((num-unresolved-deps (step-num-unresolved-deps step)))
          (set-step-score! step (+ (step-action-score step)
                                   (* score/broken num-unresolved-deps)))
          (when (= 0 num-unresolved-deps)
            (log/trace "Modifying the score of step " (step-num step)
                       " by " (num/sign score/full-solution)
                       " because it is a full solution.")
            (step-increase-score! step score/full-solution)))
        (log/trace "Updated the score of step " (step-num step)
                   " to " (step-score step))))

    (define (find-new-incipient-promotions step choice)
      (let ((triggered-promotions (find-highest-incipient-promotions-containing
                                   promotions
                                   (step-actions step)
                                   choice
                                   (step-deps-solved-by-choice step)
                                   (make-blessed-discarder step))))
        (loop ((for choice promotion (in-hashtable triggered-promotions)))
          (increase-solver-tier step promotion choice))))
    
    (define (add-new-unresolved-deps step choice)
      (let ((test-installation (extend-installation initial-state
                                                    (step-actions step))))
        (define (add-broken-deps! deps)
          (loop ((for dep (in-list deps)))
            (when (broken-under? test-installation dep)
              (add-unresolved-dep step dep))))
        (let ((new-version (choice-version choice))
              (old-version (version-of (version-package (choice-version choice))
                                       initial-state)))
          (log/trace "Finding new unresolved dependencies in step " (step-num step)
                     " caused by replacing " (dsp-version old-version)
                     " with " (dsp-version new-version) ".")
          (add-broken-deps! (version-reverse-dependencies old-version))
          (add-broken-deps! (version-reverse-dependencies new-version))
          (add-broken-deps! (version-dependencies new-version)))))

    (define (add-unresolved-dep step dep)
      (define (collect-infos+reasons versions choice-constructor)
        (loop ((for version (in-list versions))
               (let-values (solver info reason)
                 (solver-info-and-reason step dep (choice-constructor version dep #f)))
               (for infos (listing (cons solver info) (if info)))
               (for reasons (listing reason (if reason))))
          => (values infos reasons)
          (when info
            (log/trace "Adding the solver " (dsp-choice solver)
                       " with initial tier " (dsp-tier (solver-info-tier info)))
            (step-add-dep-solved-by-choice! step solver dep)
            (search-graph-bind-choice! graph solver (step-num step) dep))))
      (cond ((step-unresolved-deps-ref step dep)
             (log/trace "The dependency " (dsp-dependency dep)
                        " is already unresolved in step " (step-num step)
                        ", not adding it again"))
            (else
             (log/trace "Marking the dependency " (dsp-dependency dep)
                        " as unresolved in step " (step-num step))
             (let-values
                 (((target-infos target-reasons)
                   (collect-infos+reasons (dependency-targets dep)
                                          make-install-choice))
                  ((source-infos source-reasons)
                   (collect-infos+reasons
                    (filter (lambda (version)
                              (not (version=? version (dependency-source dep))))
                            (package-versions (version-package (dependency-source dep))))
                    make-install-from-dep-source-choice)))
               (let ((solvers (make-solver-tracker (append target-infos source-infos)
                                                   (append target-reasons source-reasons))))
                 (log/trace "Marked the dependency " (dsp-dependency dep)
                            " as unresolved in step " (step-num step)
                            " with solver list " (dsp-solver-tracker solvers))
                 (step-add-unresolved-dep! step dep solvers)
                 (find-promotions-for-dep-solvers step dep)
                 (check-solvers-tier step solvers))))))

    (define (solver-info-and-reason step dep solver)
      (assert (choice-dep solver))
      (assert (dependency=? (choice-dep solver) dep))
      (let* ((version (choice-version solver))
             (package (version-package version)))
        (cond ((choice-set-version-of (step-actions step) package)
               => (lambda (selected)
                    (when (version=? selected version)
                      (internal-error "The solver " (dsp-choice solver)
                                      " of a supposedly unresolved dependency "
                                      " is already installed in step " (step-num step)))
                    (log/trace "Not adding " (dsp-choice solver)
                               ": monotonicity violation due to "
                               (dsp-version selected))
                    (values solver #f (make-install-choice selected #f))))
              ((version=? (version-of (version-package version) initial-state)
                          version)
               (log/trace "Not adding " (dsp-choice solver)
                          ": it is the current version of the package "
                          (dsp-package package))
               (values solver #f #f))
              ((wt-tree/lookup (step-forbidden-versions step) version #f)
               => (lambda (forbidden)
                    (log/trace "Not adding " (dsp-choice solver)
                               ": it is forbidden due to the action "
                               (dsp-choice forbidden))
                    (values solver #f forbidden)))
              (else
               (values solver (solver-info solver) #f)))))

    (define (solver-info choice)
      (let ((is-deferred (build-is-deferred-listener choice)))
        (if (expression/value is-deferred)
            (make-solver-info defer-tier
                              (make-choice-set)
                              (expression/child is-deferred)
                              is-deferred)
            (make-solver-info (vector-ref version-tiers
                                          (version-id (choice-version choice)))
                              (make-choice-set)
                              #f
                              is-deferred))))

    (define (find-promotions-for-dep-solvers step dep)
      (cond ((step-unresolved-deps-ref step dep)
             => (lambda (tracker)
                  (solver-tracker-for-each
                   (lambda (solver info)
                     (find-promotions-for-solver step solver))
                   tracker)))))

    (define (find-promotions-for-solver step solver)
      (let ((output-domain (make-choice-table))
            (discarder (make-blessed-discarder step)))
        (choice-table-set! output-domain solver #t)
        (let ((triggered-promotions (find-highest-incipient-promotions-containing
                                     promotions
                                     (step-actions step)
                                     solver
                                     output-domain
                                     discarder)))
          (when (< 1 (hashtable-size triggered-promotions))
            (internal-error "Found " (hashtable-size triggered-promotions)
                            " (choice -> promotion) mappings for a single choice."))
          (loop ((for choice promotion (in-hashtable triggered-promotions)))
            (increase-solver-tier step promotion choice)))))
    
    (define (strike-structurally-forbidden step choice)
      (let ((reason (singleton-choice-set
                     (choice-with-from-dep-source? choice #f))))
        (loop ((for version (in-list (package-versions
                                      (version-package (choice-version choice))))))
          (unless (version=? version (choice-version choice))
            (log/trace "Discarding " (dsp-version version) ": monotonicity violation")
            (strike-choice step (make-install-choice version #f) reason))))
      (when (choice-from-dep-source? choice)
        (let ((version (choice-version choice))
              (reason (singleton-choice-set choice)))
          (loop ((for target (in-list (dependency-targets (choice-dep choice)))))
            (unless (version=? target version)
              (log/trace "Discarding " (dsp-version target)
                         ": forbidden by the resolution of "
                         (dsp-dependency (choice-dep choice)))
              (strike-choice step (make-install-choice target #f) reason)
              (step-add-forbidden-version! step target choice))))))
    
    (define (add-promotion step-num promotion)
      (define p (dsp-promotion promotion))
      (cond ((= 0 (choice-set-size (promotion-choices promotion)))
             (log/debug "Ignoring the empty promotion " p))
            ((promotion-set-insert! promotions promotion)
             (log/debug "Added the promotion " p
                        " to the global promotion set.")
             (promotion-queue-push! promotion-queue-tail promotion)
             (let ((new-tail (promotion-queue-next promotion-queue-tail)))
               (set! promotion-queue-tail new-tail)
               (log/debug "The promotion queue now contains "
                          (promotion-queue-index new-tail) " promotions with "
                          (promotion-queue-action-sum new-tail) " total actions.")))
            (else
             (log/debug "Did not add " p
                        " to the global promotion set: it was redundant"
                        " with an existing promotion.")))
      (search-graph-schedule-promotion-propagation! graph step-num promotion))

    (define (check-for-new-promotions step-num)
      (let* ((step (search-graph-step graph step-num))
             (current-tail promotion-queue-tail)
             (step-location (step-promotion-queue-location step))
             (tail-action-sum (promotion-queue-action-sum current-tail))
             (step-action-sum (promotion-queue-action-sum step-location))
             (action-delta (- tail-action-sum step-action-sum)))
        (log/trace
         (let ((tail-index (promotion-queue-index current-tail))
               (step-index (promotion-queue-index step-location)))
           (cat "The current promotion tail has index " tail-index
                " and action sum " tail-action-sum "; step "
                step-num " points to a promotion cell with index " step-index
                " and action sum " step-action-sum ", for a difference of "
                (- tail-index step-index) " steps and " action-delta " actions.")))
        (cond ((<= action-delta
                   (+ (choice-set-size (step-actions step))
                      (choice-table-size (step-deps-solved-by-choice step))))
               (log/trace "Applying each new promotion to step " step-num ".")
               (loop ((for entry (in-promotion-queue step-location)))
                 (apply-promotion step (promotion-queue-promotion entry)))
               (set-step-promotion-queue-location! step promotion-queue-tail))
              (else
               (receive (incipient-promotions non-incipient-promotion)
                        (find-highest-incipient-promotions
                         promotions
                         (step-actions step)
                         (step-deps-solved-by-choice step))
                 (when non-incipient-promotion
                   (log/trace "Found a new promotion in the action set of step "
                              step-num ": "
                              (dsp-promotion non-incipient-promotion))
                   (increase-step-tier step non-incipient-promotion))
                 (set-step-promotion-queue-location! step promotion-queue-tail)
                 (loop ((for choice promotion (in-hashtable incipient-promotions)))
                   (increase-solver-tier step promotion choice)))))))

    (define (apply-promotion step promotion)
      (define p (dsp-promotion promotion))
      (let ((choices (promotion-choices promotion))
            (step-num (step-num step)))
        (cond ((not (tier<? (step-tier step) (promotion-tier promotion)))
               (log/trace "Not applying " p " to step " step-num
                          ": the step tier " (dsp-tier (step-tier step))
                          " is not below the promotion tier."))
              (else
               (log/trace "Testing the promotion " p " against step " step-num)
               (let-values (((action-hits mismatch) (count-action-hits step choices))
                            ((p-size) (choice-set-size choices)))
                 (cond ((not action-hits)
                        (log/trace "Too many mismatches against " p
                                   ", not applying it."))
                       ((= action-hits p-size)
                        (log/trace "Step " step-num " contains " p
                                   " as an active promotion.")
                        (assign-step-tier step-num (step-tier step)))
                       ((< (+ action-hits 1) p-size)
                        (log/trace
                         "Step " step-num " does not contain " p "."))
                       ((not mismatch)
                        (internal-error
                         "Found an incipient promotion with no mismatches!"))
                       ((not (choice-table-contains?
                              (step-deps-solved-by-choice step)
                              mismatch))
                        (log/trace "Step " step-num " almost contains " p
                                   " as an incipient promotion, but the choice "
                                   (dsp-choice mismatch) " is not a solver."))
                       (else
                        (log/trace "Step " step-num " contains " p
                                   " as an incipient promotion for the choice "
                                   (dsp-choice mismatch) ".")
                        (increase-solver-tier step promotion mismatch))))))))

    
    (define (increase-solver-tier step promotion solver)
      (define p (dsp-promotion promotion))
      (log/trace "Applying the promotion " p " to the solver "
                 (dsp-choice solver) " in the step " (step-num step))
      (let ((new-tier (promotion-tier promotion)))
        (cond ((or (tier>=? new-tier conflict-tier)
                   (tier>=? new-tier already-generated-tier))
               (strike-choice step solver (promotion-choices promotion)))
              (else
               (let ((new-choices (choice-set-remove-overlaps
                                   (promotion-choices promotion)
                                   solver)))
                 (log/trace "Increasing the tier of " (dsp-choice solver)
                            " to " (dsp-tier new-tier)
                            " in all solver lists in step " (step-num step)
                            " with the reason set " (dsp-choice-set new-choices))
                 (choice-table-visit
                  (step-deps-solved-by-choice step)
                  solver
                  (lambda (solver solved)
                    (do-increase-tier solver
                                      solved
                                      step
                                      new-tier
                                      new-choices
                                      (promotion-valid-condition promotion))
                    #f)))))))
    
    (define (do-increase-tier solver solved step new-tier new-choices valid-condition)
      (loop ((for dep (in-list solved)))
        (let* ((solver-with-dep (choice-with-dep solver dep))
               (current-solvers (step-unresolved-deps-ref step dep))
               (info (solver-tracker-lookup current-solvers solver-with-dep)))
          (unless info
            (internal-error "In step " (step-num step) ", the solver "
                            (dsp-choice solver) "is claimed to be a solver of "
                            (dsp-dependency dep)
                            " but does not appear in its solvers list."))
          (when (tier<? (solver-info-tier info) new-tier)
            (let* ((new-info
                    (make-solver-info new-tier
                                      new-choices
                                      valid-condition
                                      (solver-info-is-deferred-listener info)))
                   (new-solvers (solver-tracker-update current-solvers
                                                       solver-with-dep
                                                       new-info)))
              (step-add-unresolved-dep! step dep new-solvers)
              (check-solvers-tier step new-solvers)
              (log/trace "Increased the tier of " (dsp-choice solver-with-dep)
                         "to " (dsp-tier new-tier) " in the solvers list of "
                         (dsp-dependency dep) " in step " (step-num step)
                         " with the reason set " (dsp-choice-set new-choices)
                         " and validity condition " (expression/dsp valid-condition)
                         " ; new solvers list: " (dsp-solver-tracker new-solvers)))))))

    (define (check-solvers-tier step solvers)
      (let ((tier (calculate-tier solvers)))
        (when (tier<? (current-search-tier) tier)
          (let ((promotion (build-promotion solvers tier)))
            (log/trace "Emitting a new promotion " (dsp-promotion promotion)
                       " at step " (step-num step))
            (add-promotion (step-num step) promotion)))
        (when (tier<? (step-tier step) tier)
          (assign-step-tier (step-num step) tier))))

    (define (assign-step-tier step-num tier)
      (let ((step (search-graph-step graph step-num)))
        (cond ((tier=? (step-tier step) tier)
               (unspecific))
              ((and (step-blessed-solution? step)
                    (tier>=? tier already-generated-tier))
               (log/trace "Step " step-num "is a blessed solution"
                          "; ignoring the attempt to promote it to tier"
                          (dsp-tier tier)))
              (else
               (log/trace "Setting the tier of step " step-num " to "
                          (dsp-tier tier))
               (let* ((was-in-pending? (wt-tree/member? step-num pending))
                      (was-in-pending-future-solutions?
                       (wt-tree/member? step-num pending-future-solutions))
                      (step-pending-count
                       (+ (if was-in-pending? 1 0)
                          (if was-in-pending-future-solutions? 1 0))))
                 (when (step-in-defer-tier? step)
                   (set! num-deferred (- num-deferred step-pending-count)))
                 (set-step-tier! step tier)
                 (when was-in-pending?
                   (wt-tree/add! pending step-num #t))
                 (when was-in-pending-future-solutions?
                   (wt-tree/add! pending-future-solutions step-num #t))
                 (cond ((step-in-defer-tier? step)
                        (set! num-deferred step-pending-count))
                       ((tier<? (step-tier step) defer-tier)
                        (when finished?
                          (set! finished? (> step-pending-count 0))))))))))

    (define (strike-choice step victim reasons)
      ;; Generalize the choice set by removing the solver that's being
      ;; struck.
      (let ((generalized-reasons (choice-set-remove-overlaps reasons victim)))
        (log/trace "Striking " (dsp-choice victim) " from all solver lists in step "
                   (step-num step) " with the reason set "
                   (dsp-choice-set generalized-reasons))
        (choice-table-visit
         (step-deps-solved-by-choice step)
         victim
         (lambda (victim solved-by-victim)
           (log/trace "Removing the choice " (dsp-choice victim)
                      " from the solver lists of ["
                      (fmt-join dsp-dependency solved-by-victim " ")
                      "] in step " (step-num step))
           (loop ((for dep (in-list solved-by-victim)))
             (let ((victim-with-dep (choice-with-dep victim dep)))
               (define (solvers-modifier current-solvers)
                 (let ((new-solvers (solver-tracker-remove current-solvers
                                                           victim-with-dep
                                                           reasons)))
                   (log/trace "Removing the choice " (dsp-choice victim-with-dep)
                              " from the solver set of " (dsp-dependency dep)
                              " in step " (step-num step)
                              ", new solvers: "
                              (dsp-solver-tracker-solvers new-solvers))
                   new-solvers))
               (search-graph-remove-choice! graph
                                            victim-with-dep
                                            (step-num step)
                                            dep)
               (cond ((step-modify-unresolved-dep! step dep solvers-modifier)
                      => (lambda (new-solvers)
                           ;; Rescan the solvers, maybe updating the step's tier.
                           (check-solvers-tier step new-solvers)))
                     (else
                      (log/trace "The dependency " (dsp-dependency dep)
                                 " has no solver set, assuming it was already solved.")))
               (log/trace "Removing all solved-by links for " (dsp-choice victim)
                          " in step " (step-num step))
               (choice-table-delete! (step-deps-solved-by-choice step) victim)
               #f ; don't stop
               ))))))

    (define (increase-step-tier step promotion)
      (let ((p-tier (promotion-tier promotion)))
        (if (tier<? (step-tier step) p-tier)
            (assign-step-tier (step-num step) p-tier))))

    (define (already-seen? step-num)
      (and-let* ((closed-step-num
                  (hashtable-ref closed (search-graph-step graph step-num) #f)))
        (cond ((= closed-step-num step-num)
               #f)
              (else
               (log/trace "Step " step-num " is irrelevant: it was already encountered"
                          " in this search.")
               (search-graph-add-clone! graph closed-step-num step-num)
               #t))))

    (define (irrelevant? step)
      (let ((tier (step-tier step)))
        (cond ((or (tier>=? tier conflict-tier)
                   (tier>=? tier already-generated-tier)
                   (< (step-score step) minimum-score))
               #t)
              (else
               (sanity-check-not-deferred step)
               #f))))

    (define (build-is-deferred-listener choice)
      (make-expression-wrapper #f))

    (define (sanity-check-promotions step)
      'FIXME)

    (define (sanity-check-not-deferred step)
      'FIXME)

    (really-make-solver find-next)))

(define (find-next-solution! solver max-steps)
  ((solver-find-next solver) max-steps))


;;; Solution

(define-record-type* solution
  (make-solution choices score tier)
  ())

(define (dsp-solution solution)
  (let ((choices (choice-set->list (solution-choices solution))))
    (cat "<" (fmt-join dsp-choice choices ", ") ">;T" (dsp-tier (solution-tier solution))
         "S" (solution-score solution))))

(define-condition-type &out-of-time-condition &condition
  make-out-of-time-condition out-of-time-condition?)


;;; Installation

(define (make-empty-installation)
  (lambda (package)
    (package-current-version package)))

(define (version-of package installation)
  (installation package))

(define (extend-installation installation choices)
  (lambda (package)
    (or (choice-set-version-of choices package)
        (version-of package installation))))

(define (broken-under? installation dependency)
  (let ((source (dependency-source dependency)))
    (cond ((not (version=? (version-of (version-package source) installation)
                           source))
           #f)
          ((or-map (lambda (target)
                     (version=? (version-of (version-package target)
                                            installation)
                                target))
                   (dependency-targets dependency))
           #f)
          (else
           #t))))


;;; Misc utilities

(define-record-type* joint-score
  (make-joint-score choices score)
  ())

(define (choice-by-action-compare c1 c2)
  (version-compare (choice-version c1) (choice-version c2)))

(define choice-by-action<? (<? choice-by-action-compare))

(define choice-by-action-wt-type (make-wt-tree-type choice-by-action<?))

(define (make-joint-score-set initial-state versions.score-list)
  (define (versions->choice-set versions)
    (loop continue
        ((for version (in-list versions))
         (with choices
               (make-choice-set)
               (choice-set-insert-or-narrow choices
                                            (make-install-choice version #f))))
      => choices
      (if (version=? version (version-of (version-package version) initial-state))
          #f
          (continue))))
  (define (add-versions-score joint-scores versions score)
    (cond ((versions->choice-set versions)
           => (lambda (choices)
                (let ((joint-score (make-joint-score choices score)))
                  (choice-set-fold
                   (lambda (choice joint-scores)
                     (wt-tree/update joint-scores
                                     choice
                                     (lambda (joint-scores)
                                       (cons joint-score joint-scores))
                                     '()))
                   joint-scores
                   choices))))
          (else
           joint-scores)))
  (loop ((for versions.score (in-list versions.score-list))
         (with joint-scores
               (make-wt-tree choice-by-action-wt-type)
               (add-versions-score joint-scores
                                   (car versions.score)
                                   (cdr versions.score))))
    => joint-scores))

(define (joint-score-set-visit joint-scores choice proc)
  (and=> (wt-tree/lookup joint-scores choice #f)
         (lambda (joint-scores)
           (loop continue ((for joint-score (in-list joint-scores)))
             (cond ((proc (joint-score-choices joint-score)
                          (joint-score-score joint-score))
                    => values)
                   (else
                    (continue)))))))

(define (make-version-scores universe version.score-list)
  (let ((version-scores (make-vector (universe-version-count universe) 0)))
    (loop ((for version.score (in-list version.score-list)))
      => version-scores
      (vector-set! version-scores
                   (version-id (car version.score))
                   (cdr version.score)))))

(define option-ref
  (let ((default-options `((step-score . 10)
                           (broken-score . 10)
                           (infinity . 10000)
                           (goal-score . 10)
                           (full-solution-score . 10)
                           (future-horizon . 50)
                           (initial-choices . ,(make-choice-set))
                           (remove-stupid? . #t)
                           (joint-scores . ())
                           (version-scores . ()))))
    (lambda (options name)
      (cond ((or (assq name options)
                 (assq name default-options))
             => cdr)
            (else
             (assertion-violation 'option-ref "invalid solver option" name))))))

(define (universe-broken-dependency-set universe installation)
  (let ((set (make-wt-tree dependency-wt-type)))
    (loop ((for dependency (in-stream (universe-dependency-stream universe))))
      => set
      (when (broken-under? installation dependency)
        (wt-tree/add! set dependency #t)))))


;; Compares steps, according to their "goodness" (the better, the
;; lower, hence the reversed ordering for the scores and actions).
(define (step-goodness-compare step1 step2)
  (refine-compare
   (tier-compare (step-tier step1) (step-tier step2))
   (number-compare (step-score step2) (step-score step1))
   (choice-set-compare (step-actions step2) (step-actions step1))))

;; Returns a wt-type that compares steps in `graph' (indexed by their
;; numbers), according to their "goodness" (the better, the lower,
;; hence the reversed ordering for the scores).
(define (search-graph->wt-tree-type graph)
  (make-wt-tree-type
   (lambda (step-num1 step-num2)
     (and (not (= step-num1 step-num2)) ;optimization
          (let ((step1 (search-graph-step graph step-num1))
                (step2 (search-graph-step graph step-num2)))
            (< (step-goodness-compare step1 step2) 0))))))

;; These two procedure replace aptitude's step_contents structure
(define (step-contents=? s1 s2)
  (and (= (step-score s1) (step-score s2))
       (= (step-action-score s1) (step-action-score s2))
       (choice-set=? (step-actions s1) (step-actions s2))))

(define (step-contents-hash step)
  (hash-combine
   (step-score step)
   (hash-combine
    (step-action-score step)
    (choice-set-hash (step-actions step)))))

(define (calculate-tier solvers)
  (loop continue ((for solver info (in-solver-tracker solvers))
                  (with tier maximum-tier))
    => tier
    (if (tier<? (solver-info-tier info) tier)
        (continue (=> tier (solver-info-tier info)))
        (continue))))

(define (build-promotion solvers tier)
  (loop ((for solver (in-solver-tracker solvers))
         (with reasons
               (make-choice-set)
               (choice-set-merge reasons (solver-info-reasons solver)))
         (for valid-conditions
              (listing (solver-info-tier-valid solver) => values)))
    => (make-promotion
        (choice-set-adjoin reasons (solver-tracker-structural-reasons solvers))
        tier
        (cond ((null? valid-conditions)
               #f)
              ((null? (cdr valid-conditions))
               (car valid-conditions))
              (else
               (make-and-expression valid-conditions))))))

(define (count-action-hits step choices)
  (let ((actions (step-actions step)))
    (loop continue ((for choice (in-choice-set choices))
                    (with hits 0)
                    (with mismatch #f))
      => (values hits mismatch)
      (cond ((choice-set-has-contained-choice? actions choice)
             (continue (=> hits (+ 1 hits))))
            ((not mismatch)
             (continue (=> mismatch choice)))
            (else
             (values #f #f))))))

(define (make-blessed-discarder step)
  (if (step-blessed-solution? step)
      (lambda (promotion)
        (tier<? (promotion-tier promotion) already-generated-tier))
      (lambda (promotion) #t)))

(define (step-in-defer-tier? step)
  (and (tier>=? (step-tier step) defer-tier)
       (tier<? (step-tier step) already-generated-tier)))

(define (num/sign n)
  (num n 10 #f #t))

)

;; Local Variables:
;; scheme-indent-styles: (foof-loop)
;; End:
