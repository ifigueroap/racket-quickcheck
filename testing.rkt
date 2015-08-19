#lang racket/base

(require (only-in rackunit define-check with-check-info fail-check))

(provide testable?
         (struct-out config)
         quick verbose check-results check quickcheck-results quickcheck check/e quickcheck/e)

(require (rename-in "generator.rkt" [bind >>=])
         "property.rkt"
         "result.rkt"
         "private/random.rkt"
         "private/glue.rkt")

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

(define-check (check/e config prop)
  (call-with-values   
   (lambda ()
     (check-results config prop))
   report-result/e))

(define (quickcheck-results prop)
  (check-results quick prop))

(define (quickcheck prop)
  (check quick prop))

(define-check (quickcheck/e prop)  
  (check/e quick prop))


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

(define (report-result/e ntest stamps maybe-result)
  (case maybe-result
    ((#t) 
     (done "OK, passed" ntest stamps))
    ((#f)
     (done "Arguments exhausted after" ntest stamps))
    (else     
     (define output-string (open-output-string))
     (for-each (lambda (x) (write-arguments x output-string))
               (result-arguments-list maybe-result))
     (with-check-info (['ntest ntest]
                       ['stamps stamps]
                       ['arguments (get-output-string output-string)])
                      (fail-check "Falsifiable")))))

; (pair (union #f symbol) value)
(define (write-argument arg [port (current-output-port)])
  (if (car arg)
      (begin
        (display (car arg) port)
        (display " = " port))
      (values))
  (write (cdr arg) port))

; (list (pair (union #f symbol) value))
(define (write-arguments args [port (current-output-port)])
  (if (pair? args)
      (begin
        (write-argument (car args) port)
        (for-each (lambda (arg)
                    (display " " port)
                    (write-argument arg port))
                  (cdr args))
        (newline))
      (values)))

(define (done mesg ntest stamps)
  (display mesg)
  (display " ")
  (display ntest)
  (display " tests")
  (let* ((sorted (sort (filter pair? stamps) stamp<?))
         (grouped (group-sizes sorted))
         (sorted (sort grouped
                       (lambda (p1 p2)
                         (< (car p1) (car p2)))))
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

(define (intersperse del lis)
  (if (null? lis)
      ""
      (string-append (car lis)
                     (let recur ((lis (cdr lis)))
                       (if (null? lis)
                           ""
                           (string-append del
                                          (recur (cdr lis))))))))