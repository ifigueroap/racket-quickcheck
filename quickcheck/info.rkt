#lang info

(define name "quickcheck")
(define scribblings '(("scribblings/quickcheck.scrbl")))

(define raco-commands
  '(("quickcheck"
     raco-quickcheck
     "autogenerate property test cases"
     25)))
