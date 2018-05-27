#!r6rs (library (spells misc compat)
  (export sleep-seconds scheme-implementation)
  (import (rnrs base)
          (only (chezscheme) sleep make-time))

  (define (sleep-seconds t)
    (let* ((seconds (exact (truncate t)))
           (fraction (- t seconds))
           (nanoseconds (exact (round (* fraction 1e+9)))))
      (sleep (make-time 'time-duration nanoseconds seconds))))

  (define (scheme-implementation)
    'chezscheme))
