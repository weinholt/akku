#!r6rs #!r6rs
;;; mit-compat.sls --- MIT compatibility shims for R6RS

;; Copyright (C) 2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(library (wak wt-tree mit-compat)
  (export define-integrable
          error:bad-range-argument
          error:wrong-type-argument)
  (import (rnrs))

(define-syntax define-integrable
  (syntax-rules ()
    ((_ id body ...)
     (define id body ...))))

(define (error:bad-range-argument value who)
  (error who "bad argument range" value))

(define (error:wrong-type-argument value expected who)
  (error who (string-append "wrong argument type, expected " expected) value))

)
