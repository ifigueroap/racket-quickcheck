#lang info

(define blurb '("DeinProgramm - QuickCheck"))
(define primary-file "main.rkt")
(define deps '("base" "rackunit"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/quickcheck.scrbl")))
(define raco-commands
  '(("quickcheck"
     quickcheck/raco-quickcheck
     "autogenerate property test cases"
     25)))
