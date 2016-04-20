#lang racket

(provide (all-defined-out))

(define (insert-every-nth n a ls)
  (define (rec i ls res)
    (if (empty? ls)
        (reverse res)
        (if (= i n)
            (rec 0 ls (cons a res))
            (rec (+ 1 i) (rest ls) (cons (first ls) res)))))
  (rec 0 ls '()))

(define (sum-of-nlist . n)
  (let ([nlist (flatten n)])
    (for/sum ([n nlist]) (expt 2 n))))

(define (sum-of-nrange ns ne)
  (for/sum ([n (in-range ns (+ ne 1))]) (expt 2 n)))


