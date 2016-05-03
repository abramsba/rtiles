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


(send map-frame show #t)

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


#|
(define layer-frame
  (new frame%
       [label "Layers"]
       [stretchable-width #f]
       [stretchable-height #f]
       [min-width 400]
       [min-height 200]))

(define lb-layers
  (new list-box%
       [label ""]
       [choices (list)]
       [parent layer-frame]))

(define layer-buttons
  (new horizontal-panel%
       [parent layer-frame]
       [alignment '(center center)]))

(define lb-add
  (new button%
       [label "Add"]
       [parent layer-buttons]))

(define lb-remove
  (new button%
       [label "Remove"]
       [parent layer-buttons]))

(send layer-frame show #t)
(send map-frame show #t)
|#
