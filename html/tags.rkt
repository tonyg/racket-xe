#lang racket/base

(require (for-syntax "../html.rkt" racket/base))
(require "../html.rkt")

(define-for-syntax (symbol-upcase sym)
  (string->symbol (string-upcase (symbol->string sym))))

(define-syntax (define/provide-html5-tags stx)
  #`(define/provide-tag-maker #,@(map symbol-upcase *html5-elements*)))

(define/provide-html5-tags)
