#lang typed/racket/base

(provide (all-defined-out))

(require racket/match)

(struct Colour
  ([red : Byte]
   [green : Byte]
   [blue : Byte])
  #:transparent)

(: hexcode->colour (-> String Colour))
(define (hexcode->colour hex)
  (if (regexp-match? #px"^[0-9A-Fa-f]{6}$" hex)
      (let ([r (string->number (substring hex 0 2) 16)]
            [g (string->number (substring hex 2 4) 16)]
            [b (string->number (substring hex 4 6) 16)])
        (with-asserts ([r byte?] [g byte?] [b byte?])
                      (Colour r g b)))
      (error "expected string of hexadecimal rgb colour code")))

(: colour->hexcode (-> Colour String))
(define (colour->hexcode colour)
  (match colour
    [(Colour r g b) (string-append (number->string r 16)
                                   (number->string g 16)
                                   (number->string b 16))]))
