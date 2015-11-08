#lang racket/base

(module+ test
  (require doc-coverage
           quickcheck
           rackunit/quickcheck)
  (check-all-documented 'quickcheck)
  (check-all-documented 'rackunit/quickcheck))
