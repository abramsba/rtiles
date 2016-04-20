#lang racket

(require json)
(require "vec.rkt")

(provide (all-defined-out))
(provide (struct-out line))

(struct line (s e) #:transparent)

(define (line-equals? l0 l1)
  (and
   (vec-equals? (line-s l0) (line-s l1))
   (vec-equals? (line-e l0) (line-e l1))))

(define (line->veclist l)
  (let
      ([v0 (line-s l)]
       [v1 (line-e l)])
    (veclist v0 v1)))

(define (linelist->veclist l)
   (flatten (build-list (length l)
              (lambda (i)
                (let ([vecs (line->veclist (list-ref l i))])
                  vecs)))))

(define (line->jsexpr l)
  (hasheq 'start (vec->jsexpr (line-s l))
          'end (vec->jsexpr (line-e l))))

(define (line-eq? . ls)
  (flatten  ls))

(define (line->vec . line)
  (let ([flat-line (flatten line)])
    (veclist (line-s line) (line-e line))))

