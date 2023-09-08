#lang racket/base

(provide intercalate)

(require racket/match)

(define (intercalate comma xs)
  (match xs
    ['() '()]
    [(list x) (list x)]
    [(list* x more) (list* x comma (intercalate comma more))]))
