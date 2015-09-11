#lang racket/base

(require racket/bool
         racket/cmdline
         raco/command-name
         "testing.rkt")


(define (run-tests-small module-path)
  (with-small-test-count
    (dynamic-require module-path #f)))

(define (run-tests-medium module-path)
  (with-medium-test-count
    (dynamic-require module-path #f)))

(define (run-tests-large module-path)
  (with-large-test-count
    (dynamic-require module-path #f)))

(define (run-tests n module-path)
  (with-test-count n
                   (dynamic-require module-path #f)))

(define (read-command-line)
  (define current-test-function (make-parameter run-tests))
  (define current-num-tests (make-parameter #f))
  (command-line
   #:program (short-program+command-name)
   #:once-any
   [("-s" "--small") "Run a small number of test cases"
                     (current-test-function run-tests-small)]
   [("-m" "--medium") "Run a medium number of test cases"
                      (current-test-function run-tests-medium)]
   [("-l" "--large") "Run a large number of test cases"
                     (current-test-function run-tests-large)]
   [("-n" "--number") n "Run <n> test cases"
                      (current-num-tests n)]
   #:args module-path
   (define (test-module-path a-module-path)
     (if (false? (current-num-tests))
         ((current-test-function) a-module-path)
         ((current-test-function) (current-num-tests) a-module-path)))
   (for-each test-module-path module-path)))

(read-command-line)
