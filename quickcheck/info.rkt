#lang info

(define name "quickcheck")
(define scribblings '(("scribblings/quickcheck.scrbl" () ("Testing"))))

(define raco-commands
  '(("quickcheck"
     quickcheck/raco-quickcheck
     "autogenerate property test cases"
     25)))

(define test-omit-paths
  '("scribblings"))
