#!r6rs #!r6rs
;;; delimited-readers.sls --- Read delimited strings.

;; Based on code snarfed from scsh, see the file AUTHORS for copyright
;; information.

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:

;;; Code:

;;@ Reading delimited strings.
(library (spells delimited-readers)
  (export read-line
	  read-paragraph
	  read-delimited
	  skip-char-set)
  (import (except (rnrs base) error string-copy string-for-each string->list)
          (rnrs io ports)
          (srfi :8 receive)
          (srfi :13 strings)
          (srfi :14 char-sets)
          (spells error)
          (spells opt-args))

;;@args delims [port delim-action]
;; Read a delimited string.
;;
;; Returns a string or the EOF object. @var{delim-action} determines what
;; to do with the terminating delimiter:
;; @table @code
;; @item 'peek
;;   Leave it in the input stream for later reading.
;; @item 'trim
;;   Drop it on the floor (the default).
;; @item 'concat
;;   Append it to the returned string.
;; @item 'split
;;   Return it as a second return value.
;; @end table
(define (read-delimited delims . args)
  (let-optionals* args ((port         (current-input-port))
                        (delim-action 'trim))
    (%read-delimited delims port delim-action)))

(define (%read-delimited delims port delim-action)
  (let* ((delims (->char-set delims))
           (eof? #f)
           (split #f)
           (result
            (string-unfold
             (lambda (c)
               (or (eof-object? c) (char-set-contains? delims c)))
             values
             (lambda (seed)
               (get-char port)
               (lookahead-char port))
             (lookahead-char port)
             ""
             (lambda (c)
               (if (eof-object? c)
                   (set! eof? #t))
               (case delim-action
                 ((peek)   "")
                 ((concat) (get-char port) (if eof? "" (string c)))
                 ((split)  (get-char port) (set! split c) "")
                 (else     (get-char port) ""))))))
      (if (and eof? (zero? (string-length result)))
          (set! result (eof-object)))
      (if split (values result split) result)))


(define charset:newline (char-set #\newline))

;;@args [ port delim-action ]
;; Read in a line of data.
;;
;; Input is terminated by either a newline or EOF.  The newline is
;; trimmed from the string by default.
(define (read-line . args)
  (let-optionals* args ((port (current-input-port))
                        (delim-action 'trim))
    (if (eq? delim-action 'trim)
        (get-line port)
        (%read-delimited charset:newline port delim-action))))


(define (blank-line? line)
  (string-every char-set:whitespace line))

;;@args [ port handle-delim ]
;; Read a paragraph.
(define (read-paragraph . args)
  (let-optionals* args ((port         (current-input-port))
                        (handle-delim 'trim))
    ;; First, skip all blank lines.
    (let lp ()
      (let ((line (read-line port 'concat)))
	(cond ((eof-object? line)
	       (if (eq? handle-delim 'split) (values line line) line))

	      ((blank-line? line) (lp))
              
	      ;; Then, read in non-blank lines.
	      (else
	       (let lp ((lines (list line)))
		 (let ((line (read-line port 'concat)))
		   (if (and (string? line)
			    (not (blank-line? line)))

		       (lp (cons line lines))

		       ;; Return the paragraph
		       (let ((lines->str
                              (lambda (lns) (apply string-append (reverse lns)))))
			 (case handle-delim
			   ((trim) (lines->str lines))

			   ((concat)
			    (lines->str
                             (if (eof-object? line) lines (cons line lines))))

			   ((split)
			    (values (lines->str lines) line))
                           
			   (else (error "Illegal HANDLE-DELIM parameter to READ-PARAGRAPH")))))))))))))
  
(define (skip-char-set skip-chars . maybe-port)
  (let* ((port (:optional maybe-port (current-input-port)))
         (cset (->char-set skip-chars)))

    (if (not (input-port? port))
      (error "Illegal value -- not an input port." port))
    
    ;; Mighty slow -- we read each char twice (peek first, then read).
    (let lp ((i 0))
      (let ((c (lookahead-char port)))
        (cond ((and (char? c) (char-set-contains? cset c))
               (get-char port)
               (lp (+ i 1)))
              (else i))))))
)
