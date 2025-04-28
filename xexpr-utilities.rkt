#lang racket/base
;; xexpr utilities, plus traversal and filtering

(provide xe
         xe*
         xa
         xa*

         attr-ref
         attr-ref/remove

         attr->classes
         classes->attr
         add-class
         add-id

         clean-xexpr-whitespace

         nodes->text

         (struct-out path-selection)
         xexpr@*
         xexpr@
         xexpr1

         ;; Axes: move around, applying filters after moving
         @=      ;; Doesn't move anywhere
         @/      ;; Moves into immediate children
         @//     ;; Flattens children recursively
         @@      ;; Moves into an immediate attribute
         @text   ;; Moves into contained text
         @xml    ;; Yields xexpr->string of selected nodes

         ;; Filters: narrow down a selection without moving
         =path   ;; Applies a nested path, keeps nodes having submatches
         =not    ;; Inverts sense of nested path, keeps nodes WITHOUT submatches
         =tag    ;; Matches a tag
         =@      ;; Matches an attribute with a particular value
         =@?     ;; Matches a present attribute
         =text   ;; Matches text
         )

(require racket/match)
(require (only-in racket/list flatten))
(require (only-in racket/string string-split string-join string-trim string-append* non-empty-string?))
(require (only-in racket/format ~a))
(require (only-in xml xexpr->string xexpr-drop-empty-attributes))

(module+ test (require rackunit))

;;---------------------------------------------------------------------------

(define-match-expander xe*
  (syntax-rules ()
    [(_ tag attrs kids)
     (or (list* (? symbol? tag) (and (or '() (cons (list _ _) _)) attrs) kids)
         (and (list* (? symbol? tag) kids)
              (app (lambda (v) '()) attrs)))])
  (syntax-rules ()
    [(_ tag attrs kids)
     (let ((a attrs))
       (if (and (null? a) (xexpr-drop-empty-attributes))
           (list* tag kids)
           (list* tag a kids)))]))

(define-match-expander xe
  (syntax-rules () [(_ tag attrs kids ...) (xe* tag attrs (list kids ...))])
  (syntax-rules () [(_ tag attrs kids ...) (xe* tag attrs (list kids ...))]))

(define-match-expander xa*
  (syntax-rules ()
    [(_ (id pat) ... rest)
     (list-no-order (list 'id pat) ... rest (... ...))]))

(define-match-expander xa
  (syntax-rules ()
    [(_ (id pat) ...) (xa* (id pat) ... _)]))

(define (attr-ref a attrs [default #f])
  (cond [(assq a attrs) => cadr]
        [(procedure? default) (default)]
        [else default]))

(define (remove-attr a attrs)
  (filter (lambda (e) (not (eq? (car e) a))) attrs))

(define (attr-ref/remove a attrs [default #f])
  (values (attr-ref a attrs default)
          (remove-attr a attrs)))

(define (attr->classes a)
  (map string->symbol (string-split a)))

(define (classes->attr cs)
  (string-join (map ~a cs) " "))

(define (add-class #:wrapper [wrapper 'span] class-or-classes item)
  (match item
    [(xe* tag attrs0 kids)
     (define-values (cs attrs) (attr-ref/remove 'class attrs0 ""))
     (xe* tag
          `((class ,(classes->attr (flatten (cons class-or-classes
                                                  (attr->classes cs)))))
            ,@attrs)
          kids)]
    [(? string? s)
     (xe wrapper `((class ,(classes->attr (flatten class-or-classes)))) s)]))

(define (add-id #:wrapper [wrapper 'span] id item)
  (match item
    [(xe* tag attrs kids)
     (xe* tag `((id ,id) ,@(remove-attr 'id attrs)) kids)]
    [(? string? s)
     (xe wrapper `((id ,id)) s)]))

(module+ test
  (check-equal? (match '((a "hi")) [(xe* _ _ _) 'is-element] [_ 'not-element]) 'not-element)
  (check-equal? (match '((a 1) (b 2) (c 3)) [(xa (b 2)) #t] [_ #f]) #t)
  (check-equal? (match '((a 1) (b 2) (c 3)) [(xa (b 1)) #t] [_ #f]) #f)
  (check-equal? (match '((a 1) (b 2) (c 3)) [(xa (b 2) (a x)) x] [_ #f]) 1)

  (check-equal? (xe 'p '()) `(p ()))
  (check-equal? (xe 'p '() "hello") `(p () "hello"))
  (check-equal? (xe 'p '() "hello" "there") `(p () "hello" "there"))
  (check-equal? (xe 'p '((id "6")) "hello" "there") `(p ((id "6")) "hello" "there"))
  (parameterize ((xexpr-drop-empty-attributes #t))
    (check-equal? (xe 'p '()) `(p))
    (check-equal? (xe 'p '() "hello") `(p "hello"))
    (check-equal? (xe 'p '() "hello" "there") `(p "hello" "there"))
    (check-equal? (xe 'p '((id "6")) "hello" "there") `(p ((id "6")) "hello" "there")))

  (check-equal? (xe* 'p '() '()) `(p ()))
  (check-equal? (xe* 'p '() '("hello")) `(p () "hello"))
  (check-equal? (xe* 'p '() '("hello" "there")) `(p () "hello" "there"))
  (check-equal? (xe* 'p '((id "6")) '("hello" "there")) `(p ((id "6")) "hello" "there"))
  (parameterize ((xexpr-drop-empty-attributes #t))
    (check-equal? (xe* 'p '() '()) `(p))
    (check-equal? (xe* 'p '() '("hello")) `(p "hello"))
    (check-equal? (xe* 'p '() '("hello" "there")) `(p "hello" "there"))
    (check-equal? (xe* 'p '((id "6")) '("hello" "there")) `(p ((id "6")) "hello" "there")))

  (check-equal? (match `(tag "text") [(xe 'tag '() "text") #t] [_ #f]) #t)
  (check-equal? (match `(tag () "text") [(xe 'tag '() "text") #t] [_ #f]) #t)
  (check-equal? (match `(tag "text2") [(xe 'tag '() "text") #t] [_ #f]) #f)

  (check-equal? (match `(tag "text") [(xe t as ks ...) (list t as ks)])
                (list 'tag '() '("text")))
  (check-equal? (match `(tag () "text") [(xe t as ks ...) (list t as ks)])
                (list 'tag '() '("text")))
  (check-equal? (match `(tag ((k "v")) "text") [(xe t as ks ...) (list t as ks)])
                (list 'tag '((k "v")) '("text")))

  (check-equal? (match `(tag "text1" "text2") [(xe t as ks ...) (list t as ks)])
                (list 'tag '() '("text1" "text2")))
  (check-equal? (match `(tag "text1" "text2") [(xe* t as ks) (list t as ks)])
                (list 'tag '() '("text1" "text2")))

  (check-equal? (attr-ref 'class '((foo "bar") (class "c") (id "123"))) "c")
  (check-equal? (attr-ref 'klass '((foo "bar") (class "c") (id "123"))) #f)
  (check-equal? (attr-ref 'klass '((foo "bar") (class "c") (id "123")) #t) #t)
  (check-equal? (attr-ref 'klass
                          '((foo "bar") (class "c") (id "123"))
                          (lambda () (+ 1 2)))
                3)
  (check-equal? (call-with-values (lambda ()
                                    (attr-ref/remove
                                     'class
                                     '((foo "bar") (class "c") (id "123"))))
                                  list)
                (list "c" '((foo "bar") (id "123"))))
  (check-equal? (call-with-values (lambda ()
                                    (attr-ref/remove
                                     'klass
                                     '((foo "bar") (class "c") (id "123"))))
                                  list)
                (list #f '((foo "bar") (class "c") (id "123")))))

;;---------------------------------------------------------------------------

(define (clean-xexpr-whitespace xexpr)
  (define (finish string-acc acc)
    (define s (string-trim (string-append* (reverse string-acc))))
    (if (non-empty-string? s)
        (cons s acc)
        acc))
  (define (clean-kids string-acc acc kids)
    (match kids
      ['()
       (reverse (finish string-acc acc))]
      [(cons (? string? s) more)
       (clean-kids (cons s string-acc) acc more)]
      [(cons other more)
       (clean-kids '() (cons (clean-xexpr-whitespace other) (finish string-acc acc)) more)]))
  (match xexpr
    [(? string? s) (string-trim s)]
    [(xe* tag attrs kids)
     (xe* tag attrs (clean-kids '() '() kids))]))

;;---------------------------------------------------------------------------

(struct path-selection (context element) #:transparent)

(define (elaborate-step step)
  (match step
    [(== /) @/] ;; shortcut
    [(== *) @=] ;; shortcut
    [(? procedure? p) p]
    ['/ @/]
    ['// @//]
    ['* @=]
    [(? symbol? tag)
     (match (symbol->string tag)
       [(regexp #px"^@(.*)$" (list _ a)) (@@ (string->symbol a))]
       [_ (=tag tag)])]
    [other (error 'xexpr@ "Invalid step: ~v" other)]))

(define (xexpr@** c e steps)
  (match e [(xe* _ _ _) 'ok] [_ (error 'xexpr@** "Invalid node: ~v" e)])
  (for/fold [(acc (list (path-selection c e)))] [(step (in-list steps))]
    (flatten (map (elaborate-step step) acc))))

(define (xexpr@* #:context [c '()] e . steps)
  (xexpr@** c e steps))

(define (xexpr@ #:context [c '()] e . steps)
  (map path-selection-element (xexpr@** c e steps)))

(define (xexpr1 #:default [default (lambda () (error 'xexpr1 "No remaining selection"))]
                #:context [c '()]
                e . steps)
  (match (xexpr@** c e steps)
    [(list (path-selection _ e)) e]
    ['() (if (procedure? default) (default) default)]
    [selections (error 'xexpr1 "Ambiguous selection (~a entries)" (length selections))]))

(define @=
  (lambda (s) s))

(define @/
  (match-lambda [(path-selection c (and e (xe* _ _ ks)))
                 (define c1 (cons e c))
                 (for/list [(k ks)] (path-selection c1 k))]
                [(path-selection _ _)
                 '()]))

(define @//
  (match-lambda [(and s (path-selection c (and e (xe* _ _ ks))))
                 (define c1 (cons e c))
                 (cons s (for/list [(k ks)] (@// (path-selection c1 k))))]
                [(path-selection _ _)
                 '()]))

(define (@@ attr)
  (match-lambda [(path-selection c (and e (xe* _ attrs _)))
                 (match (assq attr attrs)
                   [(list _ v) (path-selection (cons e c) v)]
                   [#f '()])]
                [(path-selection _ _)
                 '()]))

(define (nodes->text ks)
  (define (walk n)
    (match n
      [(xe* _ _ ks) (map walk ks)]
      [(? string? s) s]
      [(? symbol? s) (format "&~a;" s)]
      [(? number? n) (format "&#~a;" n)]))
  (string-append* (flatten (map walk ks))))

(define @text
  (match-lambda [(path-selection c (and e (xe* _ _ ks)))
                 (path-selection (cons e c) (nodes->text ks))]
                [(path-selection _ _)
                 '()]))

(define @xml
  (match-lambda [(path-selection c e)
                 (path-selection (cons e c) (xexpr->string e))]))

(define (=path . steps)
  (match-lambda [(and s (path-selection c e))
                 (if (null? (xexpr@** c e steps)) '() s)]))

(define (=not . steps)
  (match-lambda [(and s (path-selection c e))
                 (if (pair? (xexpr@** c e steps)) '() s)]))

(define (=tag t)
  (match-lambda [(and s (path-selection _ (xe* (== t) _ _))) s]
                [(path-selection _ _) '()]))

(define (text-matches? p v)
  (cond
    [(string? p) (equal? p v)]
    [(regexp? p) (regexp-match? p v)]
    [else (error 'text-matches? "Unsupported xexpr string-match pattern: ~v" p)]))

(define (=@ attr pattern)
  (match-lambda [(and s (path-selection _ (xe* _ attrs _)))
                 (match (assq attr attrs)
                   [(list _ v) (if (text-matches? pattern v) s '())]
                   [#f '()])]
                [(path-selection _ _) '()]))

(define (=@? attr)
  (match-lambda [(and s (path-selection _ (xe* _ attrs _))) #:when (assq attr attrs) s]
                [(path-selection _ _) '()]))

(define (=text p)
  (match-lambda
    [(and s (path-selection _ (xe* _ _ ks))) #:when (text-matches? p (nodes->text ks)) s]
    [(and s (path-selection _ (? string? v)))   #:when (text-matches? p v)                s]
    [(path-selection _ _) '()]))

(module+ test
  (define d1 `(AAA (BBB (CCC)
                        (www " www content " (xxx))
                        (zzz))
                   (XXX (DDD " content in ccc\n"))))
  (define d2 `(greetings (greeting "Hello World!")
                         (greeting "Ahoj Světe!")))
  (define d3 `(dataset (value ((type "int")) "42")
                       (value ((type "str")) "foo")
                       (value ((type "int")) "13")))
  (define d4 `(table
               (tr (td "a") (td "b"))
               (tr (td "c") (td "d"))))
  (define d5 `(div (p (table
                       (tr (td "a") (td "b"))
                       (tr (td "c") (td "d"))))
                   (table
                    (tr (td "e")))))
  (define d6 `(div (p (div "3")
                      (div (div "4")))))
  (define d7 `(div (p (i "3")
                      (froogy (i "4")))))
  (define d8 `(object (prop ((id "kind")) "show")
                      (prop ((id "name")) "Lucky" 9734 "Star")))
  (define d9 `(p "Hello" nbsp (testing) "world!"))

  (check-equal? (xexpr@ d2 / 'greeting) '((greeting "Hello World!") (greeting "Ahoj Světe!")))
  (check-equal? (xexpr@ d3 / 'value (=@ 'type "int"))
                '((value ((type "int")) "42") (value ((type "int")) "13")))

  (check-equal? (xexpr@ d4 'table / 'tr / 'td) `((td "a") (td "b") (td "c") (td "d")))
  (check-equal? (xexpr@ d5 @// 'td) '((td "a") (td "b") (td "c") (td "d") (td "e")))
  (check-equal? (xexpr@ d6 @// 'div)
                '((div (p (div "3") (div (div "4")))) (div "3") (div (div "4")) (div "4")))
  (check-equal? (xexpr@ d7 @// 'p / 'i) '((i "3")))

  (check-equal? (xexpr@ d8 / 'prop (=@ 'id "name") /) '("Lucky" 9734 "Star"))
  (check-equal? (xexpr@ d8 / 'prop (=@ 'id "name") @text) '("Lucky&#9734;Star"))

  (check-equal? (xexpr@ d9 @text) '("Hello&nbsp;world!"))
  (check-equal? (xexpr@ d9 @xml) '("<p>Hello&nbsp;<testing></testing>world!</p>"))
  (check-equal? (string-append* (xexpr@ d9 / @xml)) "Hello&nbsp;<testing></testing>world!")

  (check-equal? (xexpr@ `(bltx:name ()
                                    (bltx:namepart ((type "family"))
                                                   (bltx:namepart ((initial "D")) "D")
                                                   (bltx:namepart () "E"))
                                    (bltx:namepart ((initial "A") (type "given")) "A"))
                        @// @text)
                '("DEA" "DE" "D" "E" "A")))
