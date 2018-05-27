#!r6rs #!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

(library (xitomatl macro-utils)
  (export
    gen-temp syntax->list with-syntax*
    duplicate-id unique-ids? unique-ids?/raise formals-ok?/raise
    identifier-append name=? identifier?/name=?
    free-identifier-bound?)
  (import
    (rnrs)
    (only (xitomatl predicates) name=?)
    (xitomatl macro-utils fib))
  
  (define (gen-temp)
    (with-syntax (((t) (generate-temporaries '(1))))
      #'t))
     
  (define (syntax->list ls)
    (syntax-case ls ()
      ((ls ...) #'(ls ...))
      (_ (assertion-violation 'syntax->list "not a syntax list" ls))))
  
  (define-syntax with-syntax*
    (syntax-rules ()
      ((_ (pc0 pc1 pc* ...) b b* ...)
       (with-syntax (pc0)
         (with-syntax* (pc1 pc* ...) b b* ...)))
      ((_ pc b b* ...)
       (with-syntax pc b b* ...))))
  
  (define (duplicate-id ids)
    (unless (and (list? ids) (for-all identifier? ids))
      (assertion-violation 'duplicate-id "not a list of identifiers" ids))
    (let recur ((ls ids))
      (and (pair? ls)
           (let ((id (car ls)) (rest (cdr ls)))
             (if (memp (lambda (x) (bound-identifier=? x id)) rest)
               id
               (recur (cdr ls)))))))
  
  (define (unique-ids? ls)
    (not (duplicate-id ls)))
  
  (define unique-ids?/raise
    (case-lambda
      ((ids orig-stx msg)
       (let ((dup (duplicate-id ids)))
         (if dup
           (syntax-violation #F msg orig-stx dup)
           #T)))
      ((ids orig-stx)
       (unique-ids?/raise ids orig-stx "duplicate identifier"))))
  
  (define (formals-ok?/raise frmls-stx orig-stx)
    (syntax-case frmls-stx ()
      ((arg* ... . rest)
       (and (or (null? (syntax->datum #'rest))
                (identifier? #'rest)
                (syntax-violation #F "not an identifier" orig-stx #'rest))
            (for-all (lambda (id)
                       (or (identifier? id)
                           (syntax-violation #F "not an identifier" orig-stx id)))
                     #'(arg* ...))
            (unique-ids?/raise 
              (append
                #'(arg* ...)
                (if (identifier? #'rest) (list #'rest) '())) 
              orig-stx)))))
  
  (define (identifier-append ctxt . ids)
    (define who 'identifier-append)
    (unless (identifier? ctxt) (assertion-violation who "not an identifier" ctxt))    
    (let ((rs
           (apply string-append
             (map 
               (lambda (id)
                 (cond ((identifier? id) (symbol->string (syntax->datum id)))
                       ((symbol? id) (symbol->string id))
                       ((string? id) id)
                       (else (assertion-violation who 
                               "not an identifier, symbol, or string" id))))
               ids))))
      (unless (positive? (string-length rs))
        (assertion-violation who "result length zero" rs))
      (datum->syntax ctxt (string->symbol rs))))
  
  (define (identifier?/name=? id name)
    (and (identifier? id)
         (name=? id name)))

#;(define (syntax-object->source-location-expression stx)
    (let ((file-name (host:source-location-file-name stx))
          (char-pos (host:source-location-char-pos stx)))
      (assert (or (not file-name) (string? file-name)))
      (assert (or (not char-pos) (number? char-pos)))
      #`(make-source-location #,file-name #,char-pos)))
)
