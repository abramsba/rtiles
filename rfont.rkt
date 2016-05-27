#lang racket

(require json racket/draw)

(provide rfont%)

(define rfont%
  (class object%
    (super-new)
    (inspect (make-inspector))
    (field
     [size 1]
     [family 'default]
     [face "Sans"]
     [style 'normal]
     [weight 'normal]
     [underline #f]
     [smoothing 'default]
     [size-in-pixels #f]
     [hinting 'aligned]
     [offset-x 0]
     [offset-y 0]
     [font (void)])
    
    (define/public (get-size) size)
    (define/public (get-family) family)
    (define/public (get-face) face)
    (define/public (get-style) style)
    (define/public (get-weight) weight)
    (define/public (get-underline) underline)
    (define/public (get-smoothing) smoothing)
    (define/public (get-size-in-pixels) size-in-pixels)
    (define/public (get-hinting) hinting)
    (define/public (get-offset-x) offset-x)
    (define/public (get-offset-y) offset-y)

    (define/public (size! s) (set! size s))
    (define/public (family! f) (set! family f))
    (define/public (face! f) (set! face f))
    (define/public (style! s) (set! style s))
    (define/public (weight! w) (set! weight w))
    (define/public (underline! u) (set! underline u))
    (define/public (smoothing! s) (set! smoothing s))
    (define/public (size-in-pixels! s) (set! size-in-pixels s))
    (define/public (hinting! h) (set! hinting h))
    (define/public (offset-x! x) (set! offset-x x))
    (define/public (offset-y! y) (set! offset-y y))

    
    (define/private (rendered?)
      (define (check-font font)
        (and
         (= (send font get-size) size)
         (equal? (send font get-family) family)
         (equal? (send font get-face) face)
         (equal? (send font get-style) style)
         (equal? (send font get-weight) weight)
         (equal? (send font get-underlined) underline)
         (equal? (send font get-smoothing) smoothing)
         (equal? (send font get-size-in-pixels) size-in-pixels)
         (equal? (send font get-hinting) hinting)))
      (cond
        [(equal? (void) font) #f]
        [else (check-font font)]))

    (define/public (render)
      (set! font (make-font
                  #:size size
                  #:family family
                  #:face face
                  #:style style
                  #:weight weight
                  #:underlined? underline
                  #:smoothing smoothing
                  #:size-in-pixels? size-in-pixels
                  #:hinting hinting))
      font)
    
    (define/public (get-font%)
      (if (rendered?) font (send this render)))

    (define/public (->jsexpr)
      (hasheq
       'size           size
       'family         (symbol->string family)
       'face           face
       'style          (symbol->string style)
       'weight         (symbol->string weight)
       'underline      underline
       'smoothing      (symbol->string smoothing)
       'size-in-pixels size-in-pixels
       'hinting        (symbol->string hinting)
       'offset-x       offset-x
       'offset-y       offset-y))
    #|
    (field
     [size 1]
     [family 'default]
     [face "Fixed"]
     [style 'normal]
     [weight 'normal]
     [underline #f]
     [smoothing 'default]
     [size-in-pixels #f]
     [hinting 'aligned]
     [offset-x 0]
     [offset-y 0]
     [font (void)])
    |#
    ))