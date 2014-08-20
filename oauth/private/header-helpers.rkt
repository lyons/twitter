#lang typed/racket/base

(provide (all-defined-out))

(require "../parameters.rkt")

(require (only-in racket/string
                  string-replace))
(require/typed net/base64
               [base64-encode (->* [Bytes] [Bytes] Bytes)])

(: n-random-bytes (-> Natural Bytes))
(define (n-random-bytes n)
  (list->bytes
   ; typed list->bytes expects (Listof Integer) rather than (Listof Bytes), which feels dodgy
   (let loop : (Listof Integer) ([count : Natural 0]
                                 [acc : (Listof Integer) '()])
     (if (= count n)
         acc
         (loop (+ count 1)
               (cons (random 256 (oauth-pseudo-random-generator)) acc))))))

(: oauth-nonce (-> String))
(define (oauth-nonce)
  (string-replace (bytes->string/utf-8 (base64-encode (n-random-bytes 32)))
                  #px"[^[:alnum:]]"
                  ""))

(: oauth-timestamp (-> String))
(define (oauth-timestamp)
  (number->string (current-seconds)))
