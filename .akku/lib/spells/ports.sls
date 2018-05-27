#!r6rs #!r6rs
(library (spells ports)
  (export copy-port
          port->sexps
          
          make-port-tracker
          port-tracker-port
          port-tracker-column
          port-tracker-row
          port-tracker-fresh-line
          port-tracker-flush)
  (import (except (rnrs base) string-copy string-for-each string->list)
          (rnrs control)
          (rnrs bytevectors)
          (rnrs io ports)
          (only (srfi :1) unfold)
          (srfi :9 records)
          (srfi :13 strings))

  (define copy-port
    (case-lambda
      ((in-port out-port buffer-size thunk)
       (cond ((and (binary-port? in-port)
                   (binary-port? out-port))
              (let ((buffer (make-bytevector buffer-size 0)))
                (let loop ()
                  (let ((n (get-bytevector-n! in-port buffer 0 buffer-size)))
                    (cond ((not (eof-object? n))
                           (put-bytevector out-port buffer 0 n)
                           (thunk)
                           (loop)))))))
             ((and (textual-port? in-port)
                   (textual-port? out-port))
              (let ((buffer (make-string buffer-size)))
                (let loop ()
                  (let ((n (get-string-n! in-port buffer 0 buffer-size)))
                    (cond ((not (eof-object? n))
                           (put-string out-port buffer 0 n)
                           (thunk)
                           (loop)))))))
             (else
              (error 'copy-port
                     "provided ports must be both binary or both textual"
                     in-port out-port))))
      ((in-port out-port buffer-size)
       (copy-port in-port out-port buffer-size (lambda () #f)))
      ((in-port out-port)
       (copy-port in-port out-port 4000 (lambda () #f)))))

  (define (port->sexps port)
    (unfold eof-object? values (lambda (seed) (get-datum port)) (get-datum port)))


;;; Port tracker

  (define-record-type port-tracker
    (really-make-port-tracker %port %row %column)
    port-tracker?
    (port port-tracker-port port-tracker-set-port!)
    (%port %port-tracker-port)
    (%row %port-tracker-row %port-tracker-set-row!)
    (%column %port-tracker-column %port-tracker-set-column!))

  (define (%port-tracker-sync tracker)
    (flush-output-port (port-tracker-port tracker)))

  (define (port-tracker-column tracker)
    (%port-tracker-sync tracker)
    (%port-tracker-column tracker))

  (define (port-tracker-row tracker)
    (%port-tracker-sync tracker)
    (%port-tracker-row tracker))

  (define (port-tracker-flush tracker)
    (flush-output-port (port-tracker-port tracker))
    (flush-output-port (%port-tracker-port tracker)))

  (define make-port-tracker
    (case-lambda
      ((port row column)
       (let* ((tracker (really-make-port-tracker port row column))
              (port (make-custom-textual-output-port
                     "tracking ouput port"
                     (port-tracker-writer tracker)
                     (port-tracker-position-getter tracker)
                     #f
                     #f)))
         (port-tracker-set-port! tracker port)
         tracker))
      ((port)
       (make-port-tracker port 0 0))))

  (define (port-tracker-writer tracker)
    (let ((port (%port-tracker-port tracker)))
      (lambda (str start count)
        (let* ((end (+ start count))
               (nl-count (string-count str #\newline start end))
               (last-nl (string-index-right str #\newline start end)))
          (put-string port str start count)
          (%port-tracker-set-row! tracker
                                  (+ (%port-tracker-row tracker) nl-count))
          (%port-tracker-set-column! tracker
                                     (if last-nl
                                         (- count (- last-nl start) 1)
                                         (+ (%port-tracker-column tracker)
                                            count)))
          count))))

  (define (port-tracker-position-getter tracker)
    (let ((port (%port-tracker-port tracker)))
      (lambda ()
        (port-position port))))

  (define (port-tracker-fresh-line tracker)
    (unless (= (port-tracker-column tracker) 0)
      (put-char (port-tracker-port tracker) #\newline)))

  )
