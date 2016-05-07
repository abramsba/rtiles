#lang racket/gui

(require
  "zone.rkt"
  "chunk.rkt"
  "vec.rkt"
  "line.rkt")

(define target-zone (new zone% [id 'new]))

#|
Place a tile at a given location
dc = drawing context
chr = character
fnt = font
fg-c = foreground color
fg-a = foreground alpha
bg-c = background color
bg-a = background alpha
x = the X tile
y = the Y tile
w = the width of a tile
|#
(define (draw-tile dc chr fnt fg-c fg-a bg-c bg-a x y w)
  (send dc set-alpha bg-a)
  (send dc set-brush bg-c 'solid)
  (send dc set-pen "black" 1 'transparent)
  (send dc draw-rectangle (* x w) (* y w) w w)
  (send dc set-font fnt)
  (send dc set-alpha fg-a)
  (send dc set-brush fg-c 'solid)
  (send dc draw-text (string chr) x y))

#|
Erases a tile at a given location, replacing it with a black square
dc = drawing context
x = the X tile
y = the Y tile
w = the width of a tile
|#
(define (erase-tile dc x y w)
  (send dc set-brush "black" 'solid)
  (send dc draw-rectangle (* x w) (* y w) w w))

#|
Return the chunk for a layer by ID
lid = layer id
|#
(define (get-chunk lid)
  (send (send target-zone get-layer lid) chunk?))

#|
Returns the layer by the ID
lid = layer id
|#
(define (get-layer lid)
  (send target-zone get-layer lid))

#|
Creates a new layer
lid = layer id
size = amount of tiles squared
|#
(define (create-layer lid size)
  (let*
      ([chk (new chunk-mutable% [size size][data 0])]
       [lyr (new zone-layer% [id lid][chunk chk][zindex 0])])
    (send target-zone add-layer lyr)))

#|
Deletes a layer
|#
(define (remove-layer lid)
  (send target-zone remove-layer lid))


(define (mouse->vec e)
  (vec (send e get-x) (send e get-y)))

(define (vec->grid v s)
  (vec
   (truncate (/ (vec-x v) s))
   (truncate (/ (vec-y v) s))))

(define drawn #f)

(define (draw-cb dc)
  (draw-zone dc target-zone 16))

(define (draw-zone dc z ts)
  (define px (* ts 64))
  (send dc set-pen "black" 1 'transparent)
  (send dc set-brush "black" 'solid)
  (send dc draw-rectangle 0 0 px px)  
  (for ([zl (send z ->list)])
    (draw-zone-layer dc zl ts)))

(define (draw-zone-layer dc zl ts)
  (let*
      ([chk (send zl chunk?)]
       [sz (send chk size?)]
       [px (* sz ts)]
       [poslist (send chk ->veclist)]
       [fnt (make-font #:size 10 #:family 'roman)])
    (send dc set-font fnt)
    (for ([p poslist])
      (let*
          ([x (* (vec-x p) ts)]
           [y (* (vec-y p) ts)])
        (send dc set-alpha (send zl bg-alpha?))
        (send dc set-brush (send zl bg-color?) 'solid)
        (send dc draw-rectangle x y ts ts)
        (send dc set-alpha (send zl fg-alpha?))
        (send dc set-text-foreground (send zl fg-color?))
        (send dc draw-text (string (send zl character?)) x y)
        ))))

(define map-frame
  (new frame%
       [label "Map Editor - NEW"]
       [stretchable-width #t]
       [stretchable-height #t]
       [min-width 900]
       [min-height 600]))
(define map-canvas-def%
  (class canvas%
    (define/override (on-event e)
      (cond
        [(equal? #t (send e button-down? 'left))
         (displayln (vec->grid (mouse->vec e) 24))]))
    (define/override (on-char e) (display e))
    (super-new)))
(define map-canvas (new map-canvas-def%
                        [parent map-frame]
                        [style '(no-autoclear vscroll hscroll)]
                        [paint-callback (lambda (can dc) (draw-cb dc))]))
(send map-canvas init-auto-scrollbars 1024 1024 0 0)

(define main-menubar
  (new menu-bar%
       [parent map-frame]))

(define file-menu
  (new menu%
       [label "File"]
       [parent main-menubar]))

(define new-item
  (new menu-item%
       [label "New..."]
       [parent file-menu]
       [callback (lambda (menu control)
                   (send new-zone-frame show #t))]))
(define load-item
  (new menu-item%
       [label "Load..."]
       [parent file-menu]
       [callback (lambda (menu control) #f)]))

(define save-item
  (new menu-item%
       [label "Save..."]
       [parent file-menu]
       [callback (lambda (menu control) #f)]))

(define new-zone-frame
  (new frame%
       [label "New..."]
       [stretchable-width #f]
       [stretchable-height #f]))
(define new-zone-id
  (new text-field%
       [label "Zone ID:"]
       [parent new-zone-frame]))
(define new-zone-hori-frame
  (new horizontal-panel%
       [parent new-zone-frame]
       [alignment '(center center)]))
(define new-zone-ok
  (new button%
       [label "OK"]
       [parent new-zone-hori-frame]
       [callback (lambda (menu control)
                   (send new-zone-frame show #f)
                   (send map-frame set-label (format "Map Editor: ~a" (send new-zone-id get-value))))]))

(define new-zone-cancel
  (new button%
       [label "Cancel"]
       [parent new-zone-hori-frame]
       [callback (lambda (menu control) (send new-zone-frame show #f))]))

(let*
    ([cz 64]
     [c1 (new chunk-mutable% [size cz][data 0])]
     [c2 (new chunk-mutable% [size cz][data 0])]
     [c3 (new chunk-mutable% [size cz][data 0])]
     [c4 (new chunk-mutable% [size cz][data 0])]
     [c5 (new chunk-mutable% [size cz][data 0])]
     [l1 (new zone-layer% [id 'l1][chunk c1][zindex 1])]
     [l2 (new zone-layer% [id 'l2][chunk c2][zindex 2])]
     [l3 (new zone-layer% [id 'l3][chunk c3][zindex 3])]
     [l4 (new zone-layer% [id 'l5][chunk c4][zindex 4])]
     [l5 (new zone-layer% [id 'l6][chunk c5][zindex 5])]
     [zn ((lambda()
             (let ()
               (vec->chunk* c1
                            (for/list ([i (random 55 400)])
                              (vec (random cz) (random cz))))
               (vec->chunk* c2
                            (for/list ([i (random 55 400)])
                              (vec (random cz) (random cz))))
               (vec->chunk* c3
                            (for/list ([i (random 55 400)])
                              (vec (random cz) (random cz))))
               (vec->chunk* c4
                            (for/list ([i (random 55 400)])
                              (vec (random cz) (random cz))))
               (vec->chunk* c5
                            (for/list ([i (random 55 400)])
                              (vec (random cz) (random cz))))
               (send l1 character! #\+)
               (send l1 fg-color! "red")
               (send l1 bg-color! "yellow")
               (send l1 bg-alpha! 0.25)
               (send l2 character! #\#)
               (send l2 fg-color! "green")
               (send l2 bg-color! "purple")
               (send l2 bg-alpha! 0.3)
               (send l3 character! #\-)
               (send l3 fg-color! "aqua")
               (send l3 bg-color! "red")
               (send l3 bg-alpha! 0.5)
               (send l4 character! #\0)
               (send l4 fg-color! "black")
               (send l5 character! #\X)
               (send l5 fg-color! "orange")
               (send target-zone add-layer l1)
               (send target-zone add-layer l2)
               (send target-zone add-layer l3)
               (send target-zone add-layer l4)
               (send target-zone add-layer l5))))])
  (send map-frame show #t)
  (sleep/yield 1)
  (draw-cb (send map-canvas get-dc))
  )
