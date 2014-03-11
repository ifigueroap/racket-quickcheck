#lang setup/infotab

(define blurb '("DeinProgramm - QuickCheck"))
(define primary-file "main.rkt")
(define scribblings '(("scribblings/quickcheck.scrbl")))

(define compile-omit-files
  '("examples.scm"
    "packages.scm"
    "quickcheck-test.scm"
    "quickcheck.scm"
    "random.scm"))

(define deps '("base"))

