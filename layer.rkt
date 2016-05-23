#lang racket

(require
  json
  racket/draw
  "vec.rkt"
  "line.rkt"
  "utils.rkt"
  "chunk.rkt")

(provide layer% jsexpr->layer% copy-layer%)

(define layer%
  (class object%
    (init-field id [chunk (new chunk-mutable%)][zindex 0])
    (inspect (make-inspector))
    (field
     [character #\ ]
     [bg-color "black"]
     [fg-color "white"]
     [bg-alpha 1.0]
     [fg-alpha 1.0]
     [offset-x 0]
     [offset-y 0])
    (super-new)
    ; Unique symbol of this layer
    (define/public (id?) id)
    (define/public (chunk?) chunk)
    ; Change the chunk used for this layer
    (define/public (chunk! c)
      (set! chunk c))
    ; The size of this layer is the size of the chunk
    (define/public (size?) (send chunk size?))
    ; Zindex is used for sorting and rendering
    (define/public (zindex?) zindex)
    (define/public (zindex! z)
      (set! zindex z))
    ; The character drawn above the rectangle
    ; A space is not drawn
    (define/public (character?) character)
    (define/public (character! c)
      (set! character c))
    ; Is the chunk of this layer immutable
    (define/public (locked?)
      (let-values ([(cls skp) (object-info chunk)])
        (equal? cls chunk-immutable%)))
    ; Convert the chunk of this layer to mutable
    (define/public (unlock)
      (when (send this locked?)
        (set! chunk (send chunk ->mutable))))
    ; Convert the chunk of this layer to immutable
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
    ; Renders the individual tile based on properties
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
    ; Renders the entire layer 
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
    ; Convert this layer to a json expression
    (define/public (->jsexpr)
      (hasheq 'id (symbol->string id)
              'chunk (send chunk ->jsexpr)
              'zindex zindex
              'character (string character)
              'locked (send this locked?)
              'bg-color bg-color
              'bg-alpha bg-alpha
              'fg-color fg-color
              'fg-alpha fg-alpha
              'offset-x offset-x
              'offset-y offset-y))
    (define/public (<-vec . vec)
      (unless (send this locked?)
        (vec->chunk* chunk vec)))
    (define/public (<-line . line)
      (unless (send this locked?)
        (for ([l (flatten line)])
          (vec->chunk* chunk (line->veclist l)))))
    ))

(define (jsexpr->layer% json)
  (let*
      ([id (json:symbol json 'id)]
       [chunk (jsexpr->chunk%
               (json:value json 'chunk)
               (json:value json 'locked))]
       [zindex (json:value json 'zindex)]
       [bg-color (json:value json 'bg-color)]
       [bg-alpha (json:value json 'bg-alpha)]
       [fg-color (json:value json 'fg-color)]
       [fg-alpha (json:value json 'fg-alpha)]
       [offset-x (json:value json 'offset-x)]
       [offset-y (json:value json 'offset-y)]
       [character (string-ref (json:value json 'character) 0)]
       [layer (new layer%[id id][chunk chunk])])
    (send layer bg-color! bg-color)
    (send layer bg-alpha! bg-alpha)
    (send layer fg-color! fg-color)
    (send layer fg-alpha! fg-alpha)
    (send layer offset-x! offset-x)
    (send layer offset-y! offset-y)
    (send layer character! character)
    layer))

(define (copy-layer% layer)
  (let*
      ([new-layer (new layer[id (send layer id?)][chunk (send layer chunk?)][zindex (send layer zindex?)])])
    (send new-layer bg-color! (send layer bg-color?))
    (send new-layer bg-alpha! (send layer bg-alpha?))
    (send new-layer fg-color! (send layer fg-color?))
    (send new-layer fg-alpha! (send layer fg-alpha?))
    (send new-layer character! (send layer character?))
    (send new-layer offset-x! (send layer offset-x))
    (send new-layer offset-y! (send layer offset-y))
    new-layer))
       









            