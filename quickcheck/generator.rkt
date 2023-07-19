#lang racket/base

(provide (struct-out generator)
         return (rename-out [>>= bind]) sequence
         lift->generator bind-generators
         variant generate promote sized
         choose-integer choose-real choose-ascii-char choose-ascii-letter choose-printable-ascii-char
         choose-char choose-one-of choose-list choose-string choose-symbol choose-vector choose-mixed
         choose-with-frequencies)

(require "private/generator.rkt"
         "private/random.rkt"
         racket/promise
         (for-syntax racket/base
                     syntax/parse))

; int (generator a) -> (generator a)
(define (variant v gen)
  (let ((proc (generator-proc gen)))
    (make-generator
     (lambda (size rgen)
       (let loop ((v (+ 1 v))
		  (rgen rgen))
	 (if (zero? v)
	     (proc size rgen)
	     (call-with-values
		 (lambda ()
		   (random-generator-split rgen))
	       (lambda (rgen1 rgen2)
		 (loop (- v 1) rgen2)))))))))

; int random-gen (generator a) -> a
(define (generate n rgen gen)
  (call-with-values
      (lambda ()
	(random-integer rgen 0 n))
    (lambda (size nrgen)
      ((generator-proc gen) size nrgen))))

; (vals -> (generator b)) -> (generator (vals -> b))
(define (promote proc)
  (make-generator
   (lambda (size rgen)
     (lambda vals
       (let ((g (apply proc vals)))
	 ((generator-proc g) size rgen))))))

; (int -> (generator a)) -> (generator a)
(define (sized proc)
  (make-generator
   (lambda (size rgen)
     (let ((g (proc size)))
       ((generator-proc g) size rgen)))))

; [lower, upper]
(define (choose-integer lower upper)
  (make-generator
   (lambda (size rgen)
     (call-with-values
	 (lambda ()
	   (random-integer rgen lower upper))
       (lambda (n _)
	 n)))))

(define (choose-real lower upper)
  (make-generator
   (lambda (size rgen)
     (call-with-values
	 (lambda ()
	   (random-real rgen lower upper))
       (lambda (n _)
	 n)))))

(define choose-ascii-char
  (lift->generator integer->char (choose-integer 0 127)))

(define choose-ascii-letter
  (lift->generator (lambda (i)
		     (string-ref
		      "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz" i))
		   (choose-integer 0 51)))

(define choose-printable-ascii-char
  (lift->generator integer->char (choose-integer 32 127)))

(define max-scalar-value #x10FFFF)
(define gap-start #xD800)
(define gap-end #xE000)
(define gap-size (- gap-end gap-start))

(define (choose-char lower upper)
  (make-generator
   (lambda (size rgen)
     (call-with-values
	 (lambda ()
	   (random-integer rgen (char->integer lower)
			   (min (char->integer upper)
				(- max-scalar-value gap-size))))
       (lambda (n _)
	 (integer->char
	  (if (< n gap-start)
	      n
	      (+ n gap-size))))))))

; (list a) -> (generator a)
(define (choose-one-of lis)
  (lift->generator (lambda (n)
		     (list-ref lis n))
		   (choose-integer 0 (- (length lis) 1))))

; vector from the paper
; (generator a) int -> (generator (list a))
(define (choose-list el-gen n)
  (let recur ((n n))
    (if (zero? n)
	(return '())
	(>>= el-gen
	     (lambda (val)
	       (>>= (recur (- n 1))
		    (lambda (rest)
		      (return (cons val rest)))))))))

; (generator char) int -> (generator string)
(define (choose-string char-gen n)
  (lift->generator list->string (choose-list char-gen n)))

(define (choose-symbol char-gen n)
  (>>= (choose-string char-gen n)
       (lambda (s)
	 (return (string->symbol s)))))

(define (choose-vector el-gen n)
  (lift->generator list->vector (choose-list el-gen n)))

; (list (promise (generator a))) -> (generator a)
(define (choose-mixed gens)
  (>>= (choose-one-of gens)
       force))

; (list (pair int (generator a))) -> (generator a)
(define (choose-with-frequencies lis)
  (>>= (choose-integer 1 (apply + (map car lis)))
       (lambda (n)
	 (pick n lis))))

(define (pick n lis)
  (let ((k (caar lis)))
    (if (<= n k)
	(cdar lis)
	(pick (- n k) (cdr lis)))))

(define-syntax (bind-generators-recurse stx)
  (syntax-parse stx
    [(_ ([id:id val-expr:expr]
         [id-rest:id val-expr-rest:expr] ...)
        body:expr)
     #`(let* ([id val-expr]
              [gen (if (generator? id) id (generator-unit id))])
         (>>= gen (λ (id) (bind-generators-recurse
                           ([id-rest val-expr-rest] ...)
                           body))))]
    [(_ () body:expr)
     #'(return body)]))

(define-syntax (bind-generators stx)
  (syntax-parse stx
    [(_ ([id:id val-expr:expr] ...)
        body:expr)
     #'(make-generator
        (λ (size rgen)
          ((generator-proc (bind-generators-recurse
                            ([id val-expr] ...)
                            body))
           size rgen)))]))
