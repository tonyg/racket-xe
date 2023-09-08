#lang racket

(provide emit-tag
         tag-maker
         define-tag-maker
         define/provide-tag-maker

         *void-elements*
         void-element?

         *html5-elements*
         html5-element?

         html-xexpr->string

         trim-content
         )

(require (only-in xml empty-tag-shorthand xexpr->string))
(require (only-in "xexpr-utilities.rkt" xe* add-class classes->attr))

;;---------------------------------------------------------------------------

(define (emit-tag tag #:classes [classes '()] #:attrs [attrs '()] kids)
  (let* ((attrs (if (null? classes)
                    attrs
                    (cons (list 'class (classes->attr classes)) attrs))))
    (xe* tag attrs kids)))

(define ((tag-maker tag) #:classes [classes '()] #:attrs [attrs '()] . kids)
  (emit-tag tag #:classes classes #:attrs attrs kids))

(define-syntax-rule (define-tag-maker tag ...)
  (begin
    (define tag (tag-maker 'tag)) ...))

(define-syntax-rule (define/provide-tag-maker tag ...)
  (begin
    (define-tag-maker tag ...)
    (provide tag ...)))

(define *void-elements*
  '(
    ;; From https://developer.mozilla.org/en-US/docs/Glossary/Void_element :
    area
    base
    br
    col
    embed
    hr
    img
    input
    link
    meta
    param
    source
    track
    wbr

    ;; Additional, from Racket's own `html-empty-tags`:
    basefont
    frame
    isindex
    ))

(define (void-element? sym)
  (and (memq sym *void-elements*) #t))

(define *html5-elements*
  '(
    ;; List of HTML5 tags taken from https://gist.github.com/mrmrs/7650266 !
    a
    abbr
    address
    area
    article
    aside
    audio
    b
    bdi
    bdo
    blockquote
    body
    br
    button
    canvas
    caption
    cite
    code
    col
    colgroup
    command
    datalist
    dd
    del
    details
    dfn
    div
    dl
    dt
    em
    embed
    fieldset
    figcaption
    figure
    footer
    form
    h1
    h2
    h3
    h4
    h5
    h6
    header
    hr
    html
    i
    iframe
    img
    input
    ins
    kbd
    keygen
    label
    legend
    li
    main
    map
    mark
    menu
    meter
    nav
    object
    ol
    optgroup
    option
    output
    p
    param
    pre
    progress
    q
    rp
    rt
    ruby
    s
    samp
    section
    select
    small
    source
    span
    strong
    sub
    summary
    sup
    table
    tbody
    td
    textarea
    tfoot
    th
    thead
    time
    tr
    track
    u
    ul
    var
    video
    wbr
    ))

(define (html5-element? sym)
  (and (memq sym *html5-elements*) #t))

(define (html-xexpr->string xexpr)
  (parameterize ((empty-tag-shorthand *void-elements*))
    ;; ^ Void elements aren't *quite* empty-tags, but close enough.
    (xexpr->string xexpr)))

(define (trim-content #:left? [left? #t] #:right? [right? #t] items)
  (match items
    [(list) (list)]
    [(list X) (list (if (string? X) (string-trim #:left? left? #:right? right? X) X))]
    [(list L Ms ... R)
     `(,(if (string? L) (string-trim #:left? left? #:right? #f L) L)
       ,@Ms
       ,(if (string? R) (string-trim #:left? #f #:right? right? R) R))]))
