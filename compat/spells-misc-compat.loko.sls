(library (spells misc compat)
  (export
    sleep-seconds
    scheme-implementation)
  (import
    (rnrs base)
    (only (loko system fibers) sleep))

  (define (sleep-seconds t)
    (sleep t))

  (define (scheme-implementation)
    'loko))
