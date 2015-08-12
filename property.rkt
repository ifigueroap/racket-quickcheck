#lang racket

(provide property property?
         ==>
         ;label classify trivial collect
         )

(require (rename-in "generator.rkt"
                    [bind >>=])
         "private/property.rkt"
         "private/glue.rkt"
         "private/result.rkt"
         "private/text-input-output.rkt")

(define-syntax ==>
  (syntax-rules ()
    ((==> ?bool ?prop)
     (if ?bool
	 ?prop
	 (return nothing)))))

(define (label str testable)
  (>>= (coerce->result-generator testable)
       (lambda (res)
	 (return (result-add-stamp res str)))))

(define-syntax classify
  (syntax-rules ()
    ((classify ?really? ?str ?testable)
     (let ((testable ?testable))
       (if ?really?
	   (label ?str testable)
	   testable)))))

(define-syntax trivial
  (syntax-rules ()
    ((trivial ?really? ?testable)
     (classify ?really? "trivial" ?testable))))

(define (collect lbl testable)
  (label (external-representation lbl) testable))