#lang racket/base

(require (only-in rackunit define-check with-check-info with-check-info* make-check-info make-check-name fail-check))
(require quickcheck)
(require "../quickcheck/testing.rkt")

(provide check-property check-property/config add-property-check-info)

(define property-check-info (make-parameter (list)))

(define (clear-property-check-info)
  (property-check-info (list)))

(define-syntax-rule (add-property-check-info ((name value) ...))
  (property-check-info (append (property-check-info) (list (make-check-info name value) ...))))

(define-check (check-property prop)
  (clear-property-check-info)
  (let-values ([(ntest stamps maybe-result) (quickcheck/config-results (quick) prop)])
    (report-result/e ntest stamps maybe-result)))

(define-check (check-property/config config prop)
  (clear-property-check-info)
  (let-values ([(ntest stamps maybe-result) (quickcheck/config-results config prop)])
    (report-result/e ntest stamps maybe-result)))

(define (report-result/e ntest stamps maybe-result)
  (begin    
    (case maybe-result
      ((#t) 
       (done "OK, passed" ntest stamps))
      ((#f)
       (done "Arguments exhausted after" ntest stamps))
      (else     
       (define output-string (open-output-string))
       (for-each (lambda (x) (write-arguments x output-string))
                 (result-arguments-list maybe-result))
       (with-check-info* (property-check-info)
                         (lambda ()
                           (with-check-info (['ntest ntest]
                                             ['stamps stamps]
                                             ['arguments (get-output-string output-string)])                                           
                                            (fail-check "Falsifiable"))))))
    ))
