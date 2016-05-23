#lang racket

(require 
  racket/draw
  "vec.rkt"
  "chunk.rkt")

(provide (all-defined-out) layer%)

(define layer%
  (class object%
    (init-field id [chunk (new chunk-mutable%)][zindex 0])
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
    
    (define/public (->tile-bitmap ts [font (make-font #:size (/ ts 1.8) #:family 'modern)])
      (let*
          ([t_bmp (make-bitmap ts ts)]
           [t_dc (new bitmap-dc% [bitmap t_bmp])]
           [f_y (/ (send font get-size) 4.5)])
        (send t_dc set-pen "black" 0 'transparent)
        (send t_dc set-brush bg-color 'solid)
        (send t_dc set-alpha bg-alpha)
        (send t_dc draw-rectangle 0 0 ts ts)
        (send t_dc set-font font)
        (send t_dc set-text-foreground fg-color)
        (send t_dc set-alpha fg-alpha)
        (send t_dc draw-text (string character) (/ ts 4) 0)
        t_bmp))

    (define/public (->bitmap ts)
      (let*
          ([t_bmp (send this ->tile-bitmap ts)]
           [l_size (* ts (send chunk size?))]
           [l_bmp (make-bitmap l_size l_size)]
           [l_dc (new bitmap-dc% [bitmap l_bmp])])
        (for ([v (send chunk ->veclist)])
          (define-syntax-rule (vx v)
            (vec-x v))
          (define-syntax-rule (vy v)
            (vec-y v))
          (send l_dc draw-bitmap t_bmp (* (vx v) ts) (* (vy v) ts)))
        l_bmp))
    
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
