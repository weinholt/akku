#!r6rs #!r6rs
;;; string-utils.sls --- String utilities

;; Copyright (C) 2009, 2011 Andreas Rottmann <a.rottmann@gmx.at>

;; Author: Andreas Rottmann <a.rottmann@gmx.at>

;; This program is free software, you can redistribute it and/or
;; modify it under the terms of the new-style BSD license.

;; You should have received a copy of the BSD license along with this
;; program. If not, see <http://www.debian.org/misc/bsd.license>.

;;; Commentary:


;;; Code:

;;@ Utility procedures operating on strings.
(library (spells string-utils)
  (export string-split
          string-escape
          string-substitute)
  (import (except (rnrs base) string-copy string-for-each string->list)
          (rnrs control)
          (except (rnrs unicode) string-titlecase string-upcase string-downcase)
          (rnrs io simple)
          (rnrs io ports)
          (srfi :8 receive)
          (srfi :13 strings)
          (srfi :14 char-sets)
          (srfi :26 cut)
          (spells alist))


  ;;@defun string-split string charset
  ;;@defunx string-split string charset maxsplit
  ;;
  ;; Returns a list of words delimited by the characters in
  ;; @var{charset} in @var{string}. @var{charset} is a list of
  ;; characters that are treated as delimiters.  Leading or trailing
  ;; delimeters are @emph{not} trimmed. That is, the resulting list
  ;; will have as many initial empty string elements as there are
  ;; leading delimiters in @var{string}.
  ;;
  ;; If @var{maxsplit} is specified and positive, the resulting list
  ;; will contain at most @var{maxsplit} elements, the last of which
  ;; is the string remaining after (@var{maxsplit} - 1) splits. If
  ;; @var{maxsplit} is specified and non@var{-}positive, the empty
  ;; list is returned. ``In time critical applications it behooves you
  ;; not to split into more fields than you really need.''
  ;;
  ;; This is based on the split function in Python/Perl.
  ;;
  ;;@lisp
  ;; (string-split " abc d e f  ") ;==> ("abc" "d" "e" "f")
  ;; (string-split " abc d e f  " '() 1) ;==> ("abc d e f  ")
  ;; (string-split " abc d e f  " '() 0) ;==> ()
  ;; (string-split ":abc:d:e::f:" '(#\:)) ;==> ("" "abc" "d" "e" "" "f" "")
  ;; (string-split ":" '(#\:)) ;==> ("" "")
  ;; (string-split "root:x:0:0:Lord" '(#\:) 2) ;==> ("root" "x:0:0:Lord")
  ;; (string-split "/usr/local/bin:/usr/bin:/usr/ucb/bin" '(#\:))
  ;; ;==> ("/usr/local/bin" "/usr/bin" "/usr/ucb/bin")
  ;; (string-split "/usr/local/bin" '(#\/)) ;==> ("" "usr" "local" "bin")
  ;;@end lisp
  ;;@end defun
  (define (string-split str splitter . rest)
    ;; maxsplit is a positive number
    ;; str is not empty
    (define (split-by-charset str cs maxsplit)
      (define (scan-beg-word from yet-to-split-count)
        (cond
         ((>= from (string-length str)) '(""))
         ((zero? yet-to-split-count)
          (cons (substring str from (string-length str)) '()))
         (else (scan-word from from yet-to-split-count))))
      (define (scan-word i from yet-to-split-count)
        (cond
         ((>= i (string-length str))
          (cons (substring str from i) '()))
         ((char-set-contains? cs (string-ref str i))
          (cons (substring str from i)
                (scan-beg-word (+ i 1) (- yet-to-split-count 1))))
         (else (scan-word (+ i 1) from yet-to-split-count))))
      (scan-beg-word 0 (- maxsplit 1)))

    ;; resolver of overloading...
    ;; if omitted, maxsplit defaults to
    ;; (inc (string-length str))
    (if (string-null? str)
        '()
        (let ((maxsplit
               (if (pair? rest) (car rest) (+ 1 (string-length str)))))
          (cond
           ((not (positive? maxsplit)) '())
           ((pair? splitter)
            (split-by-charset str (list->char-set splitter) maxsplit))
           (else
            (split-by-charset str (->char-set splitter) maxsplit)))))
    )

  ;;@ Returns a string obtained by adding the character
  ;; @var{escape-char} before characters matching @var{to-escape},
  ;; which may be a character or a character set.  If
  ;; @var{escape-char} is not specified, @code{#\\} is used.
  (define string-escape
    (case-lambda
      ((s to-escape escape-char)
       (let ((escaped-cs (char-set-adjoin (->char-set to-escape)
                                          escape-char)))
         (string-concatenate
          (map (lambda (c)
                 (if (char-set-contains? escaped-cs c)
                     (string escape-char c)
                     (string c)))
               (string->list s)))))
      ((s to-escape)
       (string-escape s to-escape #\\))))

  ;;@ Simple template string substitution.
  (define string-substitute
    (case-lambda
      ((dst template vals grammar)
        (%string-substitute dst template vals grammar))
      ((template vals grammar)
       (string-substitute #f template vals grammar))
      ((template vals)
       (string-substitute #f template vals 'braces))))

  (define (%string-substitute dst template vals grammar)
    (define (lose msg . irritants)
      (apply error 'string-substitute msg irritants))
    (receive (open-brace close-brace)
             (case grammar
               ((braces) (values #\{ #\}))
               ((abrackets) (values #\< #\>))
               (else
                (lose "invalid grammar" dst template vals grammar)))
      (let loop ((i 0) (open-pos #f) (parts '()))
        (define (output str)
          (cond ((eqv? dst #f)
                 (cons str parts))
                ((eqv? dst #t) (display str) parts)
                (else (display str dst) parts)))
        (define (handle-close-brace/escaped pos)
          (unless (doubled-char? template pos)
            (lose "unexpected close brace" template pos))
          (loop (+ pos 2) open-pos (output (substring/shared template i (+ pos 1)))))
        (define (handle-open-brace pos)
          (cond ((doubled-char? template pos)
                 (loop (+ pos 2) #f (output
                                     (substring/shared template i (+ pos 1)))))
                (else
                 (loop (+ pos 1) pos (output
                                      (substring/shared template i pos))))))
        (if (not i)
            (if (eqv? dst #f)
                (string-concatenate-reverse parts))
            (cond (open-pos
                   (let ((close-pos (string-index template close-brace i)))
                     (unless close-pos
                       (lose "unmatched opening brace" template open-pos))
                     (cond ((doubled-char? template close-pos)
                            (loop (+ close-pos 2) open-pos parts))
                           (else
                            (loop (+ close-pos 1)
                                  #f
                                  (output (subst-one template
                                                     open-pos
                                                     close-pos
                                                     vals
                                                     lose)))))))
                  (else
                   (let ((open-pos (string-index template open-brace i))
                         (close-pos (string-index template close-brace i)))
                     (cond
                      ((not (or open-pos close-pos))
                       (loop #f #f (output (substring/shared template i))))
                      ((not open-pos)
                       (handle-close-brace/escaped close-pos))
                      ((not close-pos)
                       (handle-open-brace open-pos))
                      ((< open-pos close-pos)
                       (handle-open-brace open-pos))
                      (else
                       (handle-close-brace/escaped close-pos))))))))))

  (define (doubled-char? s i)
    (let ((c (string-ref s i)))
      (and (< (+ i 1) (string-length s))
           (char=? c (string-ref s (+ i 1))))))

  (define (subst-one template open-pos close-pos vals lose)
    (let* ((placeholder (substring/shared template (+ open-pos 1) close-pos))
           (i (or (string->number placeholder)
                  (string->symbol placeholder)))
           (val (cond ((and (vector? vals) (integer? i))
                       (vector-ref vals i))
                      ((and (integer? i) (list? vals))
                       (list-ref vals i))
                      ((symbol? i)
                       (assq-ref vals i))
                      (else
                       (lose "Invalid type for replacements" vals i)))))
      (cond ((string? val) val)
            ((number? val) (number->string val))
            ((char? val)   (string val))
            (else
             (call-with-string-output-port
               (lambda (port)
                 (display val port)))))))

  )
