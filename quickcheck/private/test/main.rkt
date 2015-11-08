#lang racket/base

(module+ property-test
  (require quickcheck
           rackunit/quickcheck)
  (define (involutes? f v) (equal? (f (f v)) v))
  (define reverse-involution?
    (property ([vs (arbitrary-list arbitrary-natural)])
      (involutes? reverse vs)))
  (quickcheck reverse-involution?)
  (check-property reverse-involution?))
