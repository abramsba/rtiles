#lang racket

(require 
  racket/draw
  "vec.rkt"
  "chunk.rkt")

(provide (all-defined-out) layer%)

(define layer%
  (class object%
    (init-field id chunk zindex)
    (inspect (make-inspector))
    (field
     [move-block #f]
     [los-block #f]
     [character #\ ]
     [bg-color "black"]
     [fg-color "white"]
     [bg-alpha 1.0]
     [fg-alpha 1.0]
     [offset-x 0]
     [offset-y 0])
    (super-new)
    (define/public (id?) id)
    (define/public (chunk?) chunk)
    (define/public (size?) (send chunk size?))
    (define/public (move-block?) move-block)
    (define/public (move-block! b)
      (set! move-block b))
    (define/public (los-block?) los-block)
    (define/public (los-block! b)
      (set! los-block b))
    (define/public (zindex?) zindex)
    (define/public (zindex! z)
      (set! zindex z))
    (define/public (character?) character)
    (define/public (character! c)
      (set! character c))
    (define/public (locked?)
      (let-values ([(cls skp) (object-info chunk)])
        (equal? cls chunk-immutable%)))
    (define/public (unlock)
      (when (send this locked?)
        (set! chunk (send chunk ->mutable))))
    (define/public (lock)
      (unless (send this locked?)
        (set! chunk (send chunk ->immutable))))
    (define/public (fg-color?) fg-color)
    (define/public (fg-color! c)
      (set! fg-color c))
    (define/public (bg-color?) bg-color)
    (define/public (bg-color! c)
      (set! bg-color c))
    (define/public (fg-alpha?) fg-alpha)
    (define/public (fg-alpha! a)
      (set! fg-alpha a))
    (define/public (bg-alpha?) bg-alpha)
    (define/public (bg-alpha! a)
      (set! bg-alpha a))
    (define/public (offset-x?) offset-x)
    (define/public (offset-x! x)
      (set! offset-x x))
    (define/public (offset-y?) offset-y)
    (define/public (offset-y! y)
      (set! offset-y y))
    (define/public (->bitmap ts)
      (let*
        ([sz (* ts (send chunk size?))]
         [bitmap (make-bitmap sz sz)]
         [dc (new bitmap-dc% [bitmap bitmap])]
         [fnt (make-font #:size (/ ts 1.4) #:family 'modern)])
        (send dc set-pen "black" 1 'transparent)
        (send dc set-brush bg-color 'solid)
        (send dc set-font fnt)
        (for ([v (send chunk ->veclist)])
          (let ([x (* ts (vec-x v))] [y (* ts (vec-y v))])
            (send dc set-alpha bg-alpha)
            (send dc draw-rectangle x y ts ts)
            (send dc set-alpha fg-alpha)
            (send dc set-text-foreground fg-color)
            (send dc draw-text (string character) (+ x 2) y)))
        bitmap))
    (define/public (->jsexpr)
      (hasheq 'id (symbol->string id)
              'chunk (send chunk ->jsexpr)
              'zindex zindex
              'locked (send this locked?)
              'move-block move-block
              'los-block los-block
              'bg-color bg-color
              'fg-color fg-color
              'offset-x offset-x
              'offset-y offset-y))))