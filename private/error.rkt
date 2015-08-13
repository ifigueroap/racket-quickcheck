#lang racket/base

(provide (struct-out exn:assertion-violation)
         assertion-violation)

(define-struct (exn:assertion-violation exn:fail) (who irritants) #:transparent)

; exceptions
(define (assertion-violation who msg . irritants)
  (raise (make-exn:assertion-violation msg (current-continuation-marks) who irritants)))