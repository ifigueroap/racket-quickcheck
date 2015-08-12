#lang racket

(provide (except-out (struct-out property)
                    property)
         (rename-out [make-property property])
         ==>
         label classify trivial collect)

;; args : (list (union arbitrary generator))
(struct property (proc arg-names args))

(define-syntax make-property
  (syntax-rules ()
    ((make-property ((?id ?gen) ...) ?body0 ?body1 ...)
     (property (lambda (?id ...)
		      ?body0 ?body1 ...)
		    '(?id ...)
		    (list ?gen ...)))))

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