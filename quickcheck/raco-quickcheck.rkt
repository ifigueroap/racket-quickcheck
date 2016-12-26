#lang racket/base

(require racket/bool
         racket/cmdline
         raco/command-name
         "testing.rkt")


(define (build-file-require the-file)
  `(file ,(if (path? the-file) (path->string the-file) the-file)))

(define (file-path->test-module-path the-file)
  (define submod-name 'property-test)
  (define sfile (build-file-require the-file))
  (define submod `(submod ,sfile ,submod-name))
  (if (module-declared? submod #t) submod sfile))

(define (test-file file-path)
  (dynamic-require (file-path->test-module-path file-path) #f))

(define (run-tests-small file-path)
  (with-small-test-count
   (test-file file-path)))

(define (run-tests-medium file-path)
  (with-medium-test-count
    (test-file file-path)))

(define (run-tests-large file-path)
  (with-large-test-count
    (test-file file-path)))

(define (run-tests n file-path)
  (with-test-count n
                   (test-file file-path)))

(define (read-command-line)
  (define current-test-function (make-parameter run-tests-small))
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
                      (current-test-function run-tests)
                      (current-num-tests n)]
   #:args file-paths
   (define (test-file-path a-file-path)
     (if (false? (current-num-tests))
         ((current-test-function) a-file-path)
         ((current-test-function) (current-num-tests) a-file-path)))
   (for-each test-file-path file-paths)))

(read-command-line)
