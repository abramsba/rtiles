#lang racket

(require
  json
  racket/draw
  "vec.rkt"
  "line.rkt"
  "rfont.rkt"
  "utils.rkt"
  "rfont.rkt"
  "chunk.rkt")

(provide layer% jsexpr->layer% copy-layer%)

(define layer%
  (class object%
    (init-field
     id
     [chunk (new chunk-mutable%)]
     [zindex 0])
    (inspect (make-inspector))
    (field
     [character #\ ]
     [bg-color "black"]
     [fg-color "white"]
     [bg-alpha 1.0]
     [fg-alpha 1.0]
     [offset-x 0]
     [offset-y 0]
     [fscale #t]
     [font (new rfont%)])
    (super-new)
    
    ; Unique symbol of this layer
    (define/public (get-id) id)
    (define/public (get-chunk) chunk)

    ; Change the chunk used for this layer
    (define/public (chunk! c)
      (set! chunk c))
    
    ; The size of this layer is the size of the chunk
    (define/public (get-size)
      (send chunk get-size))
    
    ; Zindex is used for sorting and rendering
    (define/public (get-zindex) zindex)
    (define/public (zindex! z)
      (set! zindex z))

    (define/public (get-fscale) fscale)
    (define/public (fscale! f) (set! fscale f))
    
    ; The character drawn above the rectangle
    ; A space is not drawn
    (define/public (get-character) character)
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

    ; Creates a font based on the parameters
    
    ; Convert the chunk of this layer to immutable
    (define/public (lock)
      (unless (send this locked?)
        (set! chunk (send chunk ->immutable))))
    
    (define/public (get-fg-color) fg-color)
    (define/public (fg-color! c)
      (set! fg-color c))
    
    (define/public (get-bg-color) bg-color)
    (define/public (bg-color! c)
      (set! bg-color c))
    
    (define/public (get-fg-alpha) fg-alpha)
    (define/public (fg-alpha! a)
      (set! fg-alpha a))
    
    (define/public (get-bg-alpha) bg-alpha)
    (define/public (bg-alpha! a)
      (set! bg-alpha a))
    
    (define/public (get-offset-x) offset-x)
    (define/public (offset-x! x)
      (set! offset-x x))
    
    (define/public (get-offset-y) offset-y)
    (define/public (offset-y! y)
      (set! offset-y y))

    (define/public (get-font) font)
    (define/public (font! f) (set! font f))
    
    (define/public (->tile.bmp ts)
      (let*
          ([bmp (make-bitmap ts ts)]
           [dc (new bitmap-dc% [bitmap bmp])])
        (define (drawbg)
          (hidepen dc)
          (send dc set-brush bg-color 'solid)
          (send dc set-alpha bg-alpha)
          (send dc draw-rectangle 0 0 ts ts))
        (define (drawfg)
          (send dc set-text-foreground fg-color)
          (send dc set-alpha fg-alpha)
          (when fscale
            (send font size! (/ ts 1.5)))
          (send dc set-font (send font render))
          (send dc draw-text (string character)
                (send font get-offset-x) (send font get-offset-y)))
        (drawbg)
        (drawfg)
        (when (not (zero? bg-alpha)) (drawbg))
        (when (not (or (zero? fg-alpha) (equal? #\  character))) (drawfg))
        bmp))

    (define/public (->layer.bmp ts)
      (let*
          ([tile.bmp (send this ->tile.bmp ts)]
           [size (* ts (send chunk get-size))]
           [layer.bmp (make-bitmap size size)]
           [dc (new bitmap-dc% [bitmap layer.bmp])])
        (for ([v (send chunk ->vecs)])
          (send dc draw-bitmap tile.bmp
                (* (vec-x v) ts) (* (vec-y v) ts)))
        layer.bmp))

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
      ([new-layer (new layer[id (send layer id?)][chunk (send layer chunk?)][zindex (send layer get-zindex)])])
    (send new-layer bg-color! (send layer bg-color?))
    (send new-layer bg-alpha! (send layer bg-alpha?))
    (send new-layer fg-color! (send layer fg-color?))
    (send new-layer fg-alpha! (send layer fg-alpha?))
    (send new-layer character! (send layer character?))
    (send new-layer offset-x! (send layer offset-x))
    (send new-layer offset-y! (send layer offset-y))
    new-layer))








            