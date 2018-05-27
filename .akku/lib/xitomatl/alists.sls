#!r6rs #!r6rs
;; Copyright 2009 Derick Eddington.  My MIT-style license is in the file named
;; LICENSE from the original collection this file is distributed with.

;; This library provides the (rnrs hashtables) API for association lists.
;; The procedures starting with alist- operate on the alist record type
;; defined in this library.  The procedures not starting with alist-
;; operate on raw association lists and are functional (never mutate).
;; The order of association lists is preserved for all operations.

(library (xitomatl alists)
  (export
    alist? pred-alist? equal-alist? eqv-alist? eq-alist?
    make-pred-alist make-equal-alist make-eqv-alist make-eq-alist
    alist-ref assp-ref assoc-ref assv-ref assq-ref
    alist-set! assp-replace assoc-replace assv-replace assq-replace
    alist-delete! assp-remove assoc-remove assv-remove assq-remove
    alist-update! assp-update assoc-update assv-update assq-update
    alist-copy ass-copy alist-clear!
    alist-keys ass-keys alist-entries ass-entries
    alist-contains? alist-size alist-equivalence-function alist-mutable?)
  (import
    (rnrs)
    (only (xitomatl define) define/? define/who))
  
  (define-record-type alist 
    (fields (mutable al) m))
  (define (make-alist-subtype-protocol who)
    (lambda (p)
      (letrec ((f (case-lambda 
                    (() (f '() #T))
                    ((al) (f al #T))
                    ((al m) 
                     (unless (and (list? al) (for-all pair? al))
                       (assertion-violation who "not a list of pairs" al))
                     (unless (boolean? m)
                       (assertion-violation who "not a boolean" m))
                     ((p al m))))))
        f)))
  (define-record-type pred-alist (parent alist) 
    (protocol (make-alist-subtype-protocol 'make-pred-alist)))
  (define-record-type equal-alist (parent alist) 
    (protocol (make-alist-subtype-protocol 'make-equal-alist)))
  (define-record-type eqv-alist (parent alist) 
    (protocol (make-alist-subtype-protocol 'make-eqv-alist)))
  (define-record-type eq-alist (parent alist) 
    (protocol (make-alist-subtype-protocol 'make-eq-alist)))
  
  (define (make-dispatch asspf assocf assvf assqf)
    (lambda (a) 
      (cond ((eq-alist? a) assqf)
            ((equal-alist? a) assocf)
            ((eqv-alist? a) assvf)
            ((pred-alist? a) asspf))))
  
  (define-syntax alist-al-set!/check-immutable--macro
    (syntax-rules ()
      ((_ who a-expr al-expr)
       (let ((a a-expr))
         (check-mutability a who)
         (alist-al-set! a al-expr)))))
  
  (define (check-mutability a who)
    (unless (alist-m a)
      (assertion-violation who "alist is immutable" a)))
  
  (define-syntax define-alist-proc
    (lambda (stx)
      (syntax-case stx ()
        ((ctxt name (asspf assocf assvf assqf) expr) 
         (identifier? #'name)
         (with-syntax ((d (datum->syntax #'ctxt 'dispatch))
                       (s (datum->syntax #'ctxt 'alist-al-set!/check-immutable)))
           #'(define/? name
               (let ((d (make-dispatch asspf assocf assvf assqf)))
                 (let-syntax ((s (syntax-rules ()
                                   ((_ a al) 
                                    (alist-al-set!/check-immutable--macro 'name a al))))) 
                   expr))))))))
  
  (define (not-proper who x) 
    (assertion-violation who "not a proper alist" x))
  
  (define (not-found who kop al)
    (assertion-violation who "key not found" kop #;al))
  
  (define (apply-p p key)
    (p key))
  
  (define (ignore who kop al)
    (values))
  
  ;--------------------------------------------------------------------------
  
  (define/? (alist-size (a alist?))
    (length (alist-al a)))
  
  ;--------------------------------------------------------------------------
  
  (define (ass-ref--meta in-al kop default pred who nf)
    (let loop ((al in-al))
      (cond ((and (pair? al) (pair? (car al)))
             (if (pred kop (caar al))
               (cdar al)
               (loop (cdr al))))
            ((null? al)
             (nf who kop in-al)
             default)
            (else (not-proper who in-al)))))
  
  (define (make-ass-ref pred who)
    (case-lambda
      ((al kop default)
       (ass-ref--meta al kop default pred who ignore))
      ((al kop)
       (ass-ref--meta al kop #F pred who not-found))))
  
  (define/who assp-ref (make-ass-ref apply-p who))
  (define/who assoc-ref (make-ass-ref equal? who))
  (define/who assv-ref (make-ass-ref eqv? who))
  (define/who assq-ref (make-ass-ref eq? who))
  
  (define-alist-proc alist-ref 
                     (assp-ref assoc-ref assv-ref assq-ref)
    (case-lambda/?
      (((a alist?) kop d)
       ((dispatch a) (alist-al a) kop d))
      (((a alist?) kop) 
       ((dispatch a) (alist-al a) kop))))
  
  ;--------------------------------------------------------------------------
  
  (define (make-ass-replace pred nf who)
    (lambda (al kop obj)
      (ass-update--meta al kop (lambda (_) obj) #F pred nf who)))
  
  (define/who assp-replace (make-ass-replace apply-p not-found who))
  (define/who assoc-replace (make-ass-replace equal? ignore who))  
  (define/who assv-replace (make-ass-replace eqv? ignore who))
  (define/who assq-replace (make-ass-replace eq? ignore who))
  
  (define-alist-proc alist-set! 
                     (assp-replace assoc-replace assv-replace assq-replace)
    (lambda/? ((a alist?) kop o)
      (alist-al-set!/check-immutable a ((dispatch a) (alist-al a) kop o))))
  
  ;--------------------------------------------------------------------------
  
  (define (ass-remove--meta in-al kop pred who)
    (let loop ((al in-al) (new '()))
      (cond ((and (pair? al) (pair? (car al)))
             (if (pred kop (caar al))
               (loop (cdr al) new)
               (loop (cdr al) (cons (car al) new))))
            ((null? al)
             (reverse new))
            (else (not-proper who in-al)))))
  
  (define (make-ass-remove pred who)
    (lambda (al kop)
      (ass-remove--meta al kop pred who)))
  
  (define/who assp-remove (make-ass-remove apply-p who))
  (define/who assoc-remove (make-ass-remove equal? who))
  (define/who assv-remove (make-ass-remove eqv? who))
  (define/who assq-remove (make-ass-remove eq? who))
 
  (define-alist-proc alist-delete!
                     (assp-remove assoc-remove assv-remove assq-remove)
    (lambda/? ((a alist?) kop) 
      (alist-al-set!/check-immutable a ((dispatch a) (alist-al a) kop))))
  
  ;--------------------------------------------------------------------------
  
  (define does-not-contain (list #T))  ;; make a unique object
  
  (define (make-ass-contains? pred who)
    (lambda (al kop)
      (not (eq? (ass-ref--meta al kop does-not-contain pred who ignore)
                does-not-contain))))
  
  (define/who assp-contains? (make-ass-contains? apply-p who))
  (define/who assoc-contains? (make-ass-contains? equal? who))
  (define/who assv-contains? (make-ass-contains? eqv? who))
  (define/who assq-contains? (make-ass-contains? eq? who))
  
  (define-alist-proc alist-contains? 
                     (assp-contains? assoc-contains? assv-contains? assq-contains?)
    (lambda/? ((a alist?) kop)
      ((dispatch a) (alist-al a) kop)))
  
  ;--------------------------------------------------------------------------
  
  (define (ass-update--meta in-al kop proc default pred nf who)
    (let loop ((al in-al) (new '()) (found #F))
      (cond ((and (pair? al) (pair? (car al)))
             (if (pred kop (caar al))
               (loop (cdr al) (cons (cons (caar al) (proc (cdar al))) new) #T)
               (loop (cdr al) (cons (car al) new) found)))
            ((null? al)
             (if found 
               (reverse new) 
               (begin (nf who kop in-al)
                      (reverse (cons (cons kop (proc default)) new)))))
            (else (not-proper who in-al)))))
  
  (define (make-ass-update pred nf/dflt nf who)
    (case-lambda
      ((al kop proc default)
       (ass-update--meta al kop proc default pred nf/dflt who))
      ((al kop proc)
       (ass-update--meta al kop proc #F pred nf who))))
  
  (define/who assp-update (make-ass-update apply-p not-found not-found who))  
  (define/who assoc-update (make-ass-update equal? ignore not-found who))  
  (define/who assv-update (make-ass-update eqv? ignore not-found who))
  (define/who assq-update (make-ass-update eq? ignore not-found who))
  
  (define-alist-proc alist-update!
                     (assp-update assoc-update assv-update assq-update)
    (case-lambda/?
      (((a alist?) kop (p procedure?) d)
       (alist-al-set!/check-immutable a ((dispatch a) (alist-al a) kop p d)))
      (((a alist?) kop (p procedure?))
       (alist-al-set!/check-immutable a ((dispatch a) (alist-al a) kop p)))))
  
  ;--------------------------------------------------------------------------

  (define (ass-copy in-al)
    (let loop ((al in-al) (new '()))
      (cond ((and (pair? al) (pair? (car al)))
             (loop (cdr al) (cons (cons (caar al) (cdar al)) new)))
            ((null? al)
             (reverse new))
            (else (not-proper 'ass-copy in-al)))))
  
  (define-alist-proc alist-copy 
                     (make-pred-alist make-equal-alist make-eqv-alist make-eq-alist)
    (case-lambda/?
      ((a) (alist-copy a #F))
      (((a alist?) mutable)
       ((dispatch a) (ass-copy (alist-al a)) (and mutable #T)))))
  
  ;--------------------------------------------------------------------------
  
  (define/? (alist-clear! (a alist?))
    (alist-al-set!/check-immutable--macro 'alist-clear! a '()))
  
  ;--------------------------------------------------------------------------
  
  (define (ass-keys in-al) 
    (let loop ((al in-al) (keys '()))
      (cond ((and (pair? al) (pair? (car al)))
             (loop (cdr al) (cons (caar al) keys)))
            ((null? al)
             (reverse keys))
            (else (not-proper 'ass-keys in-al)))))
  
  (define/? (alist-keys (a alist?))
    (list->vector (ass-keys (alist-al a))))
  
  ;--------------------------------------------------------------------------
  
  (define (ass-entries in-al) 
    (let loop ((al in-al) (keys '()) (vals '()))
      (cond ((and (pair? al) (pair? (car al))) 
             (loop (cdr al) (cons (caar al) keys) (cons (cdar al) vals)))
            ((null? al) 
             (values (reverse keys) (reverse vals)))
            (else (not-proper 'ass-entries in-al)))))
  
  (define/? (alist-entries (a alist?))
    (let-values (((kl vl) (ass-entries (alist-al a))))
      (values (list->vector kl) (list->vector vl))))
  
  ;--------------------------------------------------------------------------

  (define-alist-proc alist-equivalence-function
                     ('pred equal? eqv? eq?)
    (lambda/? ((a alist?))
      (dispatch a)))
  
  (define/? (alist-mutable? (a alist?))
    (alist-m a))

)
