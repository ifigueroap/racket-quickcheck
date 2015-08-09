#lang racket

(require (rename-in "generator.rkt"
                    [bind >>=])
         "private/error.rkt")

;; generator   : (generator a)
;; transformer : a (generator b) -> (generator b)
(define-struct arbitrary (generator transformer))

; class Arbitrary a where
;    arbitrary   :: Gen a
;    coarbitrary :: a -> Gen b -> Gen b

(define (coarbitrary arb val gen)
  ((arbitrary-transformer arb) val gen))

(define arbitrary-boolean
  (make-arbitrary (choose-one-of '(#t #f))
		  (lambda (a gen)
		    (variant (if a 0 1) gen))))

(define arbitrary-integer
  (make-arbitrary (sized
		   (lambda (n)
		     (choose-integer (- n) n)))
		  (lambda (n gen)
		    (variant (if (>= n 0)
				 (* 2 n)
				 (+ (* 2 (- n)) 1))
			     gen))))

(define arbitrary-natural
  (make-arbitrary (sized
		   (lambda (n)
		     (choose-integer 0 n)))
		  (lambda (n gen)
		    (variant n gen))))

(define arbitrary-ascii-char
  (make-arbitrary choose-ascii-char
		  (lambda (ch gen)
		    (variant (char->integer ch) gen))))

(define arbitrary-ascii-letter
  (make-arbitrary choose-ascii-letter
		  (lambda (ch gen)
		    (variant (char->integer ch) gen))))

(define arbitrary-printable-ascii-char
  (make-arbitrary choose-printable-ascii-char
		  (lambda (ch gen)
		    (variant (char->integer ch) gen))))

(define arbitrary-char
  (make-arbitrary (sized
		   (lambda (n)
		     (choose-char (integer->char 0)
				  (integer->char n))))
		  (lambda (ch gen)
		    (variant (char->integer ch) gen))))

(define (make-rational a b)
  (/ a
     (+ 1 b)))

(define arbitrary-rational
  (make-arbitrary (lift->generator make-rational
				   (arbitrary-generator arbitrary-integer)
				   (arbitrary-generator arbitrary-natural))
		  (lambda (r gen)
		    (coarbitrary arbitrary-integer
				 (numerator r)
				 (coarbitrary arbitrary-integer
					      (denominator r) gen)))))

(define (fraction a b c)
  (+ a
     (exact->inexact (/ b
			(+ (abs c) 1)))))

(define arbitrary-real
  (make-arbitrary (lift->generator fraction
				   (arbitrary-generator arbitrary-integer)
				   (arbitrary-generator arbitrary-integer)
				   (arbitrary-generator arbitrary-integer))
		  (lambda (r gen)
		    (let ((fr (rationalize (inexact->exact r) 1/1000)))
		      (coarbitrary arbitrary-integer
				   (numerator fr)
				   (coarbitrary arbitrary-integer
						(denominator fr) gen))))))


(define (arbitrary-mixed pred+arbitrary-promise-list)
  (make-arbitrary (choose-mixed (map (lambda (p)
				       (delay (arbitrary-generator (force (cdr p)))))
				     pred+arbitrary-promise-list))
		  (lambda (val gen)
		    (let loop ((lis pred+arbitrary-promise-list) (n 0))
		      (cond
		       ((null? lis)
			(assertion-violation 'arbitrary-mixed
					     "value matches none of the predicates"
					     val pred+arbitrary-promise-list))
		       (((caar lis) val)
			(variant n gen))
		       (else
			(loop (cdr lis) (+ 1 n))))))))

(define (arbitrary-one-of eql? . vals)
  (make-arbitrary (choose-one-of vals)
		  (lambda (val gen)
		    (let loop ((lis vals) (n 0))
		      (cond
		       ((null? lis)
			(assertion-violation 'arbitrary-one-of
					     "value is not in the list"
					     val vals))
		       ((eql? (car lis) val)
			(variant n gen))
		       (else
			(loop (cdr lis) (+ 1 n))))))))
		       
(define (arbitrary-pair arbitrary-car arbitrary-cdr)
  (make-arbitrary (lift->generator cons
				   (arbitrary-generator arbitrary-car)
				   (arbitrary-generator arbitrary-cdr))
		  (lambda (p gen)
		    (coarbitrary arbitrary-car
				 (car p)
				 (coarbitrary arbitrary-cdr
					      (cdr p) gen)))))

; a tuple is just a non-uniform list 
(define (arbitrary-tuple . arbitrary-els)
  (make-arbitrary (apply lift->generator
			 list
			 (map arbitrary-generator arbitrary-els))
		  (lambda (lis gen)
		    (let recur ((arbitrary-els arbitrary-els)
				(lis lis))
		      (if (null? arbitrary-els)
			  gen
			  ((arbitrary-transformer (car arbitrary-els))
			   (car lis)
			   (recur (cdr arbitrary-els)
				  (cdr lis))))))))

(define (arbitrary-record construct accessors . arbitrary-els)
  (make-arbitrary (apply lift->generator
			 construct
			 (map arbitrary-generator arbitrary-els))
		  (lambda (rec gen)
		    (let recur ((arbitrary-els arbitrary-els)
				(lis (map (lambda (accessor) (accessor rec)) accessors)))
		      (if (null? arbitrary-els)
			  gen
			  ((arbitrary-transformer (car arbitrary-els))
			   (car lis)
			   (recur (cdr arbitrary-els)
				  (cdr lis))))))))

(define (arbitrary-sequence choose-sequence sequence->list arbitrary-el)
  (make-arbitrary (sized
		   (lambda (n)
		     (>>= (choose-integer 0 n)
			  (lambda (length)
			    (choose-sequence (arbitrary-generator arbitrary-el) length)))))
		  (lambda (seq gen)
		    (let recur ((lis (sequence->list seq)))
		      (if (null? lis)
			  (variant 0 gen)
			  ((arbitrary-transformer arbitrary-el) 
			   (car lis)
			   (variant 1 (recur (cdr lis)))))))))

(define (arbitrary-list arbitrary-el)
  (arbitrary-sequence choose-list values arbitrary-el))

(define (arbitrary-vector arbitrary-el)
  (arbitrary-sequence choose-vector vector->list arbitrary-el))

(define arbitrary-ascii-string
  (arbitrary-sequence choose-string string->list arbitrary-ascii-char))

(define arbitrary-printable-ascii-string
  (arbitrary-sequence choose-string string->list arbitrary-printable-ascii-char))

(define arbitrary-string
  (arbitrary-sequence choose-string string->list arbitrary-char))

(define arbitrary-symbol
  (arbitrary-sequence choose-symbol
		      (lambda (symbol)
			(string->list (symbol->string symbol)))
		      arbitrary-ascii-letter))

(define (arbitrary-procedure arbitrary-result . arbitrary-args)
  (let ((arbitrary-arg-tuple (apply arbitrary-tuple arbitrary-args)))
    (make-arbitrary (promote
		     (lambda args
		       ((arbitrary-transformer arbitrary-arg-tuple)
			args
			(arbitrary-generator arbitrary-result))))
		    (lambda (proc gen)
		      (>>= (arbitrary-generator arbitrary-arg-tuple)
			   (lambda (args)
			     ((arbitrary-transformer arbitrary-result)
			      (apply proc args)
			      gen)))))))