#lang setup/infotab

(define blurb '("DeinProgramm - QuickCheck"))
(define primary-file "main.rkt")
(define deps '("base"))
(define build-deps '("scribble-lib" "racket-doc"))
(define scribblings '(("scribblings/quickcheck.scrbl")))

(define compile-omit-files
  '("examples.scm"
    "packages.scm"
    "quickcheck-test.scm"
    "quickcheck.scm"
    "random.scm"))
