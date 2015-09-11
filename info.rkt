#lang info

(define collection 'multi)

(define version "0.1")

(define deps '("base"
               "rackunit"))

(define build-deps '("scribble-lib"
                     "racket-doc"))

(define raco-commands
  '(("quickcheck"
     quickcheck/raco-quickcheck
     "autogenerate property test cases"
     25)))
