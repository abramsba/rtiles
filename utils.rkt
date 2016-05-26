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

(define (list-of-nrange ns ne)
  (for/list ([n (in-range ns (+ ne 1))]) (expt 2 n)))

(define (bytes->number bytes)
  (define length (bytes-length bytes))
  (let loop ([n 0] [factor 1] [num 0])
    (if (= n length)
        num
        (loop (+ 1 n)
              (* factor 256)
              (+ num (* (bytes-ref bytes n)
                        factor))))))
 
(define (number->bytes byte-length num)
  (define bytes (make-bytes byte-length))
  (let loop ([n 0] [num num])
    (if (= n byte-length)
        bytes
        (let-values ([(quot rem) (quotient/remainder num 256)])
          (bytes-set! bytes n rem)
          (loop (+ 1 n) quot)))))


; https://www.reddit.com/r/qeddit/comments/14qra9/racket_decimal_binary_conversion_function/c7fkgsh
(define (number->binary-string n)
 (cond [(< n 2) (number->string n)]
    [else (string-append (number->binary-string (quotient n 2)) (number->string (remainder n 2)))]))

; http://stackoverflow.com/questions/15871042/how-do-i-find-the-index-of-an-element-in-a-list-in-racket
(define (index-of-car lst ele)
  (let loop([lst lst] [idx 0])
    (cond ([empty? lst] #f)
          ([equal? (car (first lst)) ele] idx)
          (else (loop (rest lst) (add1 idx))))))

(define (index-of-cdr lst ele)
  (let loop([lst lst] [idx 0])
    (cond ([empty? lst] #f)
          ([equal? (cdr (first lst)) ele] idx)
          (else (loop (rest lst) (add1 idx))))))

(define (get-random lst)
  (car
   (shuffle
    lst)))

(define (json:symbol json key)
  (string->symbol (hash-ref json key)))

(define (json:value json key)
  (hash-ref json key))

(define (hidepen dc)
  (send dc set-pen "black" 0 'transparent) dc)

