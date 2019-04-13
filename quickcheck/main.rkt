#lang racket/base

(provide quickcheck quickcheck-results make-config
         
         (struct-out result)
         arbitrary?
         arbitrary
         (struct-out generator)
         (struct-out config)
         
         with-test-count
         with-small-test-count
         with-medium-test-count
         with-large-test-count
         
         choose-integer choose-real
         choose-ascii-char choose-ascii-letter choose-printable-ascii-char choose-char
         choose-list choose-vector choose-string choose-symbol
         (rename-out [return generator-unit]
                     [bind generator-bind]
                     [sequence generator-sequence])
         bind-generators
         sized choose-one-of choose-mixed choose-with-frequencies
         arbitrary-boolean arbitrary-char arbitrary-ascii-char arbitrary-printable-ascii-char
         arbitrary-integer arbitrary-natural arbitrary-rational arbitrary-real
         arbitrary-mixed arbitrary-one-of
         arbitrary-pair
         arbitrary-list
         arbitrary-vector
         arbitrary-tuple arbitrary-record
         arbitrary-string
         arbitrary-ascii-string arbitrary-printable-ascii-string
         arbitrary-symbol
         arbitrary-procedure
         property
         property?
         ==>
         label
         classify
         trivial
         collect
         testable?
         )

(require "arbitrary.rkt"
         "generator.rkt"
         "property.rkt"
         "result.rkt"
         "testing.rkt"
         "private/random.rkt"
         "private/error.rkt")
