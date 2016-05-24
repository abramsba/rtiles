#lang racket

(require data/bit-vector)

(require racket/draw "utils.rkt" "vec.rkt")

(provide (all-defined-out) chunk-immutable% chunk-mutable%)

(define chunk-base%
  (class object%
    (init-field [size 8][data 0])
    (inspect (make-inspector))
    (super-new)
    (define/public (size?) size)
    (define/public (data?) data)
    (define/public (bits?) (expt size 2))
    (define/public (alive?) (not (= 0 data)))

    (define/public (->nlist)
      (let iter ([n 0]
                 [output '()])
        (if (= n (send this bits?))
            (reverse output)
            (iter (+ n 1)
                  (if (< 0 (bitwise-and (expt 2 n) data))
                      (cons n output)
                      output)))))

    (define/public (->veclist)
      (map (λ (v)
             (let-values ([(y x) (quotient/remainder v size)])
               (vec x y)))
           (send this ->nlist)))
    
    (define/public (equal? chunk)
      (and (= (send chunk size?) size) (= (send chunk data?) data)))
    (abstract or)
    (abstract and)
    (abstract invert)
    (abstract fill)
    (abstract empty)
    (define/public (->jsexpr)
      (hasheq 'size size 'data data))
    (define/public (->list)
      (list size data))))

(define chunk-mutable%
  (class chunk-base%
    (inherit-field size data)
    (inspect (make-inspector))
    (super-new)
    (define/public (->immutable)
      (new chunk-immutable% [size size][data data]))
    (define/public (data* d) (set! data d))
    (define/override (or . chunks)
      (let* ([flat-chunks (flatten chunks)]
             [datalist
              (flatten
               (list (send this data?) (map (lambda (c) (send c data?)) flat-chunks)))]
             [output
              (apply bitwise-ior datalist)])
        (set! data output)))
    (define/override (and . chunks)
      (let* ([flat-chunks (flatten chunks)]
             [datalist
              (flatten
               (list (send this data?) (map (lambda (c) (send c data?)) flat-chunks)))]
             [output
              (apply bitwise-and datalist)])
        (set! data output)))
    (define/override (invert)
      (let ([rng (sum-of-nrange 0 (send this bits?))])
        (set! data
              (bitwise-xor rng data))))
    (define/override (fill)
      (set! data (sum-of-nrange 0 (send this bits?))))
    (define/override (empty) (set! data 0))))

(define chunk-immutable%
  (class chunk-base%
    (super-new)
    (inherit-field size data)
    (inspect (make-inspector))
    (define/public (->mutable)
      (new chunk-mutable% [size size][data data]))
    (define/override (or . chunks)
      (let* ([flat-chunks (flatten chunks)]
             [datalist
              (flatten
               (list (send this data?) (map (lambda (c) (send c data?)) flat-chunks)))]
             [output
              (apply bitwise-ior datalist)])
        (new chunk-mutable% [size size][data output])))
    (define/override (and . chunks)
      (let* ([flat-chunks (flatten chunks)]
             [datalist
              (flatten
               (list (send this data?) (map (lambda (c) (send c data?)) flat-chunks)))]
             [output
              (apply bitwise-and datalist)])
        (new chunk-mutable% [size size][data output])))
    (define/override (invert)
      (let ([rng (sum-of-nrange 0 (send this bits?))])
        (new chunk-immutable% [size size][data (bitwise-xor rng data)])))
    (define/override (fill)
      (new chunk-immutable% [size size][data (sum-of-nrange 0 (send this bits?))]))
    (define/override (empty)
      (new chunk-immutable% [size size][data 0]))))

(define (vec->chunk size mutable . vec)
  (let*
    ([flat-vec (flatten vec)]
     [vec-res (apply + (map (lambda (v) (expt 2 (+ (* size (vec-y v)) (vec-x v)))) flat-vec))])
    (if mutable
      (new chunk-mutable% [size size][data vec-res])
      (new chunk-immutable% [size size][data vec-res]))))

(define (vec->chunk! chk . vec)
  (let*
      ([flat-vec (flatten vec)]
       [size (send chk size?)]
       [vec-res (apply bitwise-ior (map (lambda (v) (expt 2 (+ (* size (vec-y v)) (vec-x v)))) flat-vec))])
    (send chk data* vec-res)))

(define (vec->chunk* chk . vec)
  (let*
      ([flat-vec (flatten vec)]
       [size (send chk size?)]
       [vec-res (apply + (map (lambda (v) (expt 2 (+ (* size (vec-y v)) (vec-x v)))) flat-vec))])
    (send chk data* (bitwise-ior vec-res (send chk data?)))))

(define (jsexpr->chunk% json mutable)
  (let*
      ([size (json:value json 'size)]
       [data (json:value json 'data)])
    (if mutable
        (new chunk-immutable% [size size] [data data])
        (new chunk-mutable% [size size] [data data]))))
