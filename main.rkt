#lang racket/base

(provide check check-results make-config
	 quickcheck quickcheck-results
	 check-result? result-arguments-list
	 choose-integer choose-real
	 choose-ascii-char choose-ascii-letter choose-printable-ascii-char choose-char
	 choose-list choose-vector choose-string choose-symbol
	 generator-unit generator-bind generator-sequence
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
     arbitrary-generator
     make-arbitrary
     make-generator
	 property
	 property?
	 ==>
	 label
	 classify
	 trivial
	 collect
	 )

(require "quickcheck.rkt"
	 "random.rkt")

(provide exn:assertion-violation?
	 exn:assertion-violation-who
	 exn:assertion-violation-irritants)
