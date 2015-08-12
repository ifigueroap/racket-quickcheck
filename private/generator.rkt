#lang racket

(provide (struct-out generator)
         return >>= sequence lift->generator)

(require "random.rkt")

;; proc : int(size) random-generator -> val
(define-struct generator (proc))

; for transliteration from Haskell
(define (return val)
  (make-generator
   (lambda (size rgen)
     val)))

(define (>>= m1 k)
  (let ((proc1 (generator-proc m1)))
    (make-generator
     (lambda (size rgen)
       (call-with-values
	   (lambda ()
	     (random-generator-split rgen))
	 (lambda (rgen1 rgen2)
	   (let ((gen (k (proc1 size rgen1))))
	     ((generator-proc gen) size rgen2))))))))

(define (sequence gens)
  (if (null? gens)
      (return '())
      (>>= (car gens)
	   (lambda (val)
	     (>>= (sequence (cdr gens))
		  (lambda (rest)
		    (return (cons val rest))))))))

(define (lift->generator proc . gens)
  (>>= (sequence gens)
       (lambda (vals)
	 (return (apply proc vals)))))