#lang racket/gui

(require json "vec.rkt" "line.rkt" "chunk.rkt" "layer.rkt" "zone.rkt")

(provide (all-defined-out))

(define (load-zone-file-prompt)
  (define file (get-file
   "Load .zone File"
   #f
   (current-directory)
   #f
   ".zone"
   '(packages)
   (list (list "Zone" "*.zone"))))
  (unless (eq? #f file)
    (define if (open-input-file file))
    (define if_string (read-line if))
    (close-input-port if)
    (jsexpr->zone% (string->jsexpr if_string))))

(define (save-zone-file-prompt zone)
  (let
      ([out (jsexpr->string (send zone ->jsexpr))])
    (define file (put-file
     "Save .zone File"
     #f
     (current-directory)
     (format "~a.zone" (symbol->string (send zone get-id)))
     ".zone"
     '(packages)
     (list (list "Zone" "*.zone"))))
    (unless (eq? #f file)
      (define of (open-output-file file #:exists 'replace))
      (display out of)
      (close-output-port of))))

(define (random-color)
  (car (shuffle (send the-color-database get-names))))

(define (random-unicode-char)
  (integer->char (random #x2580 #x259F)))

(define (random-noise size density)
  (let*
      ([amount (/ (* size size) density)])
    (for/list ([i amount])
      (vec (random size) (random size)))))

(define (random-line-noise size density)
  (flatten (for/list ([i (random (/ size density))])
    (line->veclist (line (vec (random size) (random size)) (vec (random size) (random size)))))))

(define (random-layer size density id)
  (let*
      ([chunk (new chunk-mutable%[size size])]
       [layer (new layer% [id id][chunk chunk])])
    (send layer <-vec (random-noise size density))
    (send layer fg-alpha! (random))
    (send layer bg-alpha! (random))
    (send layer fg-color! (random-color))
    (send layer bg-color! (random-color))
    (send (send layer get-font) offset-x! (random 16))
    (send layer character! (random-unicode-char))
    layer))

(define (random-zone size layers density)
  (let*
      ([zone (new zone%[id 'test])])
    (for ([i layers])
      (let*
          ([id (string->symbol (format "zl_~a" i))]
           [ly (random-layer size density id)])
        (send (send ly get-font) weight! 'bold)
        (send zone add-layer ly)))
    zone))