#!r6rs #!r6rs
;;; gc.sls --- Interface to the implementation's GC

;; Copyright (C) 2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:


;;@ This library exposes an interface to implementation's garbage
;; collector.
(library (spells gc)
  (export make-reaper
          make-weak-cell weak-cell-ref weak-cell?
          collect)
  (import (rnrs base)
          (rnrs control)
          (spells misc)
          (spells gc compat))

;;@subheading Resource reclamation
;;
;; The following procedure allows for registration of a set of objects
;; with the garbage collector, and the execution of an action should
;; they become eligible for garbage collection.

;;@defun make-reaper proc
;;
;; Returns a single procedure (a ``reaper'') that accepts zero or,
;; alternatively, one argument.  When passed a single argument, the
;; reaper will register the argument with the garbage collector.  In
;; this case, the reaper returns unspecified values.
;;
;; Calling the reaper with zero arguments may cause @var{proc} to be
;; applied to an object that has been registered with the garbage
;; collector via the same reaper, and has become ripe for garbage
;; collection (i.e. it has become invisible except to the reaper).
;; The call to the reaper returns the values returned by its
;; invocation to @var{proc}, or @code{#f} if @var{proc} was not
;; invoked since no registered object was eligible for collection.
;;
;;@end defun

;;@subheading Weak cells
;;
;; Weak cells are single-value containers referencing some value that
;; may be garbage collected, even though referenced by the weak cell.
;; When the value is indeed garbage collected, the reference inside
;; all weak cells referring to it is "broken", and
;; @code{weak-cell-ref} for such cells will return @code{#f}.

;;@defun make-weak-cell object
;;
;; Create a weak cell referencing @var{object}.
;;
;;@end defun

;;@defun weak-cell-ref weak-cell
;;
;; Return the value contained in @var{object}, or @code{#f} if the
;; value was garbage collected.
;;
;;@end defun

;;@defun weak-cell? object
;;
;; Returns @code{#t} if @var{object} is a weak cell. Note that
;; disjointness of weak cells is not guaranteed.
;;
;;@end defun

;;@subheading Triggering garbage collection
;;
;; Note that triggering garbage collection is something that should
;; happen automatically; this interface is provided as a debugging and
;; testing aid, and should not be used in ``regular'' code.
;;

;;@defun collect
;;
;; Trigger a run of the garbage collector.
;;
;;@end defun

)
