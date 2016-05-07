#lang racket/gui

(require
  "zone.rkt"
  "chunk.rkt"
  "vec.rkt"
  "line.rkt")

(define (mouse->vec e)
  (vec (send e get-x) (send e get-y)))

(define (vec->grid v s)
  (vec
   (truncate (/ (vec-x v) s))
   (truncate (/ (vec-y v) s))))

(define (draw-zone dc z ts)
  (for ([zl (send z ->list)])
    (draw-zone-layer dc zl ts)))

(define (draw-zone-layer dc zl ts)
  (let*
      ([chk (send zl chunk?)]
       [sz (send chk size?)]
       [px (* sz ts)]
       [poslist (send chk ->veclist)]
       [fnt (make-font #:size 8)])
    (for ([p poslist])
      (let*
          ([x (* (vec-x p) ts)]
           [y (* (vec-y p) ts)])
        (send dc set-text-foreground (send zl fg-color?))
        (send dc draw-text (string (send zl character?)) x y)
        ))))

#|
  The new file dialog
|#

(define map-frame
  (new frame%
       [label "Map Editor - NEW"]
       [stretchable-width #f]
       [stretchable-height #f]
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
(define map-canvas (new map-canvas-def% [parent map-frame]))

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
    ([c1 (new chunk-mutable% [size 32][data 0])]
     [c2 (new chunk-mutable% [size 32][data 0])]
     [c3 (new chunk-mutable% [size 32][data 0])]
     [c4 (new chunk-mutable% [size 32][data 0])]
     [c5 (new chunk-mutable% [size 32][data 0])]
     [l1 (new zone-layer% [id 'l1][chunk c1][zindex 1])]
     [l2 (new zone-layer% [id 'l2][chunk c2][zindex 2])]
     [l3 (new zone-layer% [id 'l3][chunk c3][zindex 3])]
     [l4 (new zone-layer% [id 'l5][chunk c4][zindex 4])]
     [l5 (new zone-layer% [id 'l6][chunk c5][zindex 5])]
     [zn ((lambda()
             (let
                 ([newz (new zone%[id 'test])])
               (vec->chunk* c1
                            (for/list ([i (random 15 100)])
                              (vec (random 1 31) (random 1 31))))
               (vec->chunk* c2
                            (for/list ([i (random 15 100)])
                              (vec (random 1 31) (random 1 31))))
               (vec->chunk* c3
                            (for/list ([i (random 15 100)])
                              (vec (random 1 31) (random 1 31))))
               (vec->chunk* c4
                            (for/list ([i (random 15 100)])
                              (vec (random 1 31) (random 1 31))))
               (vec->chunk* c5
                            (for/list ([i (random 15 100)])
                              (vec (random 1 31) (random 1 31))))
               (send l1 character! #\#)
               (send l1 fg-color! "red")
               (send l2 character! #\u039E)
               (send l2 fg-color! "green")
               (send l3 character! #\u0414)
               (send l3 fg-color! "aqua")
               (send l4 character! #\uFEB6)
               (send l4 fg-color! "white")
               (send l5 character! #\u03BB)
               (send l5 fg-color! "orange")
               (send newz add-layer l1)
               (send newz add-layer l2)
               (send newz add-layer l3)
               (send newz add-layer l4)
               (send newz add-layer l5)
               newz)))])
  (define bmp (make-bitmap (* 32 16) (* 32 16)))
  (draw-zone (new bitmap-dc% [bitmap bmp]) zn 16)
  bmp)
