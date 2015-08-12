#lang racket

(provide (struct-out arbitrary)
         coarbitrary)

;; generator   : (generator a)
;; transformer : a (generator b) -> (generator b)
(define-struct arbitrary (generator transformer))

; class Arbitrary a where
;    arbitrary   :: Gen a
;    coarbitrary :: a -> Gen b -> Gen b

(define (coarbitrary arb val gen)
  ((arbitrary-transformer arb) val gen))