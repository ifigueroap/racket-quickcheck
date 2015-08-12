#lang racket

(provide (except-out (struct-out property) property)
         (rename-out [make-property property]))

;; args : (list (union arbitrary generator))
(struct property (proc arg-names args))

(define-syntax make-property
  (syntax-rules ()
    [(make-property ((?id ?gen) ...) ?body0 ?body1 ...)
     (property (lambda (?id ...)
                 ?body0 ?body1 ...)
               '(?id ...)
               (list ?gen ...))]))