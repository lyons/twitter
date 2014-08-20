#lang typed/racket/base

(require "../private/header-helpers.rkt")

(require racket/match
         typed/rackunit)

;; n-random-bytes appears to return n bytes
(check-eq? (bytes-length (n-random-bytes 0))
           0)

(let loop ([n 0])
  (if (< n 1000)
      (let ([size (random 65536)])
        (check-eq? (bytes-length (n-random-bytes size))
                   size
                   "n-random-bytes should produce n random bytes")
        (loop (+ n 1)))
      #t))

;; n-random-bytes appears random
(: uniform-length-random-bytes (Listof Bytes))
(define uniform-length-random-bytes
  (let loop ([n 0] [acc (ann '() : (Listof Bytes))])
    (if (< n 25000)
        (loop (+ n 1) (cons (n-random-bytes 256) acc))
        acc)))

(define sorted-uniform-length-random-bytes
  (sort uniform-length-random-bytes bytes<?))

(: check-adjacent-values-differ (-> (Listof Bytes) Boolean))
(define (check-adjacent-values-differ ls)
  (match ls
    [(list-rest a b tail) (check-not-equal? a b "n-random-bytes shouldn't produce duplicates")
                          (check-adjacent-values-differ (cons b tail))]
    [_ #t]))

(check-adjacent-values-differ sorted-uniform-length-random-bytes)
