#lang racket/base

(provide external-representation)

; extended-ports
(define make-string-output-port open-output-string)
(define string-output-port-output get-output-string)

(define (external-representation obj)
  (let ((port (make-string-output-port)))
    (write obj port)
    (string-output-port-output port)))