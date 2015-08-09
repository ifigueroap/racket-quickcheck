#lang racket/base

; QuickCheck clone

(require racket/promise
         "random.rkt")

(provide (all-defined-out))

; extended-ports
(define make-string-output-port open-output-string)
(define string-output-port-output get-output-string)

; sorting
(define (list-sort < lis)
  (sort lis <))


;; args : (list (union arbitrary generator))
(define-struct property (proc arg-names args) #:omit-define-syntaxes)

(define-syntax property
  (syntax-rules ()
    ((property ((?id ?gen) ...) ?body0 ?body1 ...)
     (make-property (lambda (?id ...)
		      ?body0 ?body1 ...)
		    '(?id ...)
		    (list ?gen ...)))))

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

; A testable value is one of the following:
; - a :property object
; - a boolean
; - a result record
; - a generator of a result record

(define (testable? thing)
  (or (property? thing)
      (boolean? thing)
      (result? thing)
      (generator? thing)))

(define (coerce->result-generator thing)
  (cond
   ((property? thing)
    (for-all/names (property-proc thing)
		   (property-arg-names thing)
		   (property-args thing)))
   ((boolean? thing) (return (result-with-ok nothing thing)))
   ((result? thing) (return thing))
   ((generator? thing) thing)
   (else
    (assertion-violation 'coerce->result-generator 
			 "cannot be coerced to a result generator"
			 thing))))

(define (coerce->generator thing)
  (cond
   ((generator? thing) thing)
   ((arbitrary? thing) (arbitrary-generator thing))
   (else
    (assertion-violation 'coerce->generator
			 "cannot be coerced to a generator" thing))))

(define (for-all proc . args)
  (>>= (sequence (map coerce->generator args))
       (lambda (args)
	 (>>= (coerce->result-generator (apply proc args))
	      (lambda (res)
		(return (result-add-arguments res
					      (map (lambda (arg) (cons #f arg)) args))))))))

(define (for-all/names proc arg-names args)
  (>>= (sequence (map coerce->generator args))
       (lambda (args)
	 (>>= (coerce->result-generator (apply proc args))
	      (lambda (res)
		(return (result-add-arguments res (map cons arg-names args))))))))

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
 
(define (external-representation obj)
  (let ((port (make-string-output-port)))
    (write obj port)
    (string-output-port-output port)))

; Running the whole shebang

(define-struct config (max-test max-fail size print-every))

(define quick
  (make-config 100
	       1000
	       (lambda (n)
		 (+ 3 (quotient n 2)))
	       values))

(define verbose
  (make-config 100
	       1000
	       (lambda (n)
		 (+ 3 (quotient n 2)))
	       (lambda (n args)
		 (display n)
		 (display ":")
		 (newline)
		 (for-each (lambda (arg)
			     (display arg)
			     (newline))
			   args))))

(define (check-results config prop)
  (let ((rgen (make-random-generator 0)))
    (tests config (coerce->result-generator prop) rgen 0 0 '())))

(define (check config prop)
  (call-with-values
      (lambda ()
	(check-results config prop))
    report-result))

(define (quickcheck-results prop)
  (check-results quick prop))

(define (quickcheck prop)
  (check quick prop))

; returns three values:
; - ntest
; - stamps
; - #t for success, #f for exhausted, result for failure

(define (tests config gen rgen ntest nfail stamps)
  (let loop ((rgen rgen)
	     (ntest ntest)
	     (nfail nfail)
	     (stamps stamps))
    (cond
     ((= ntest (config-max-test config))
      (values ntest stamps #t))
     ((= nfail (config-max-fail config))
      (values ntest stamps #f))
     (else
      (call-with-values
	  (lambda ()
	    (random-generator-split rgen))
	(lambda (rgen1 rgen2)
	  (let ((result (generate ((config-size config) ntest) rgen2 gen)))
	    ((config-print-every config) ntest (result-arguments-list result))
	    (case (result-ok result)
	      ((()) (loop rgen1 ntest (+ 1 nfail) stamps))
	      ((#t) (loop rgen1 (+ 1 ntest) nfail (cons (result-stamp result) stamps)))
	      ((#f)
	       (values ntest stamps result))))))))))

(define (report-result ntest stamps maybe-result)
  (case maybe-result
    ((#t)
     (done "OK, passed" ntest stamps))
    ((#f)
     (done "Arguments exhausted after" ntest stamps))
    (else
     (display "Falsifiable, after ")
     (display ntest)
     (display " tests:")
     (newline)
     (for-each write-arguments
	       (result-arguments-list maybe-result)))))

; (pair (union #f symbol) value)
(define (write-argument arg)
  (if (car arg)
      (begin
	(display (car arg))
	(display " = "))
      (values))
  (write (cdr arg)))

; (list (pair (union #f symbol) value))
(define (write-arguments args)
  (if (pair? args)
      (begin
	(write-argument (car args))
	(for-each (lambda (arg)
		    (display " ")
		    (write-argument arg))
		  (cdr args))
	(newline))
      (values)))
		   
(define (done mesg ntest stamps)
  (display mesg)
  (display " ")
  (display ntest)
  (display " tests")
  (let* ((sorted (list-sort stamp<? (filter pair? stamps)))
	 (grouped (group-sizes sorted))
	 (sorted (list-sort (lambda (p1 p2)
			      (< (car p1) (car p2)))
			    grouped))
	 (entries (map (lambda (p)
			 (let ((n (car p))
			       (lis (cdr p)))
			 (string-append (number->string (quotient (* 100 n) ntest))
					"% "
					(intersperse ", " lis))))
		       (reverse sorted))))
    (cond
     ((null? entries)
      (display ".")
      (newline))
     ((null? (cdr entries))
      (display " (")
      (display (car entries))
      (display ").")
      (newline))
     (else
      (display ".") (newline)
      (for-each (lambda (entry)
		  (display entry)
		  (display ".")
		  (newline))
		entries)))))

(define (group-sizes lis)
  (if (null? lis)
      '()
      (let loop ((current (car lis))
		 (size 1)
		 (lis (cdr lis))
		 (rev '()))
	(cond
	 ((null? lis)
	  (reverse (cons (cons size current) rev)))
	 ((equal? current (car lis))
	  (loop current (+ 1 size) (cdr lis) rev))
	 (else
	  (loop (car lis) 1 (cdr lis) (cons (cons size current) rev)))))))

(define (stamp<? s1 s2)
  (cond
   ((null? s1)
    (pair? s1))
   ((null? s2)
    #t)
   ((string<? (car s1) (car s2))
    #t)
   ((string=? (car s1) (car s2))
    (stamp<? (cdr s1) (cdr s2)))
   (else #f)))


(define (intersperse del lis)
  (if (null? lis)
      ""
      (string-append (car lis)
		     (let recur ((lis (cdr lis)))
		       (if (null? lis)
			   ""
			   (string-append del
					  (recur (cdr lis))))))))
