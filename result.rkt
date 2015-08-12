#lang racket

;; ok             : () = unknown, #t, #f
;; arguments-list : (list (list (pair (union #f symbol) value)))
(define-struct result (ok stamp arguments-list))

(define (result-with-ok res ok)
  (make-result ok
	       (result-stamp res)
	       (result-arguments-list res)))

(define (result-add-stamp res stamp)
  (make-result (result-ok res)
	       (cons stamp (result-stamp res))
	       (result-arguments-list res)))

; result (list (pair (union #f symbol) value)) -> result
(define (result-add-arguments res args)
  (make-result (result-ok res)
	       (result-stamp res)
	       (cons args (result-arguments-list res))))

(define nothing
  (make-result '() '() '()))