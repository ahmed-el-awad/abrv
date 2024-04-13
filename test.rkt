#lang racket

(define % remainder)

(% 5 3)

(define (bior param1 param2)
  (or param1 param2))

(bior #f #f)
