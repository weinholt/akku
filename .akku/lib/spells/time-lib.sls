#!r6rs #!r6rs
;;; time-lib.sls --- Time library.

;; Copyright (C) 2009, 2010, 2011, 2015 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

(library (spells time-lib)
  (export posix-timestamp->time-utc
          time-utc->posix-timestamp
          time-utc->posix-offset
          date-up-from
          date-down-from)
  (import
    (rnrs base)
    (srfi :19 time)
    (spells opt-args))

  (define *posix-epoch* (date->time-utc (make-date 0 0 0 0 1 1 1970 0)))

  (define* (posix-timestamp->time-utc timestamp (nanoseconds 0))
    (add-duration *posix-epoch* (make-time time-duration nanoseconds timestamp)))

  (define (time-utc->posix-timestamp time-utc)
    (time-second (time-utc->posix-offset time-utc)))

  (define (time-utc->posix-offset time-utc)
    (time-difference time-utc *posix-epoch*))

  (define one-day (make-time time-duration 0 (* 24 60 60)))
  
  (define-syntax date-up-from
    (syntax-rules ()
      ((_ (date-var) (start-expr (to end-expr)) cont . env)
       (cont
        (((end) (date->time-utc end-expr)) ;Outer bindings
         ((start tz) (let ((start start-expr))
                       (values start (date-zone-offset start))))
         ((step) one-day))
        ((time-var (date->time-utc start)  ;Loop variables
                   (add-duration time-var step)))
        ()                                 ;Entry bindings
        ((time>=? time-var end))           ;Termination conditions
        (((date-var)                       ;Body bindings
          (time-utc->date time-var tz)))
        ()                                 ;Final bindings
        . env))))
  
  (define-syntax date-down-from
    (syntax-rules ()
      ((_ (date-var) (start-expr (to end-expr)) cont . env)
       (cont
        (((end) (date->time-utc end-expr)) ;Outer bindings
         ((start tz) (let ((start start-expr))
                       (values start (date-zone-offset start))))
         ((step) one-day))
        ((time-var (date->time-utc start)  ;Loop variables
                   (subtract-duration time-var step)))
        ()                                 ;Entry bindings
        ((time<=? time-var end))           ;Termination conditions
        (((date-var)                       ;Body bindings
          (time-utc->date time-var tz)))
        ()                                 ;Final bindings
        . env))))
  
)
