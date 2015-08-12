#lang racket/base

; QuickCheck clone

(require "arbitrary.rkt"
         (rename-in "generator.rkt"
                    [bind >>=])
         "private/random.rkt"
         "property.rkt"
         "private/error.rkt"
         "testing.rkt"
         "result.rkt"
         racket/promise)

(provide (all-defined-out))

; sorting
(define (list-sort < lis)
  (sort lis <))





