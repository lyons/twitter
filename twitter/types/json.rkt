#lang typed/racket/base

(provide JSON JSON-Null json-hash
         write-json json->string json->bytes
         read-json string->json bytes->json)

(define-type JSON
  (Rec JSExpr (U 'null
                 Boolean
                 String
                 Integer
                 Inexact-Real
                 (Listof JSExpr)
                 (HashTable Symbol JSExpr))))

(: JSON-Null (-> JSON))    ; Typed hash-ref only accepts #f or a function as failure value
(define (JSON-Null) 'null) ; Which is total BS

(require/typed json
  [(write-json write-json!)
   (->* [JSON]
        [Output-Port #:null Any #:encode (U 'control 'all)]
        Void)]
  [jsexpr->string
   (->* [JSON]
        [#:null Any #:encode (U 'control 'all)]
        String)]
  [jsexpr->bytes
   (->* [JSON]
        [#:null Any #:encode (U 'control 'all)]
        Bytes)]

  [(read-json read-json!)
   (->* []
        [Input-Port #:null Any]
        (U JSON EOF))]
  [string->jsexpr
   (->* [String]
        [#:null Any]
        JSON)]
  [bytes->jsexpr
   (->* [Bytes]
        [#:null Any]
        JSON)])

(require/typed racket/function
  [(identity json-hash) (-> Any (HashTable Symbol JSON))])

(: write-json
   (->* [JSON]
        [Output-Port #:encode (U 'control 'all)]
        Void))
(define write-json
  (λ (json
      [port (current-output-port)]
      #:encode [encode 'control])
    (write-json! json port #:null 'null #:encode encode)))

(: json->string
   (->* [JSON]
        [#:encode (U 'control 'all)]
        String))
(define json->string
  (λ (json
      #:encode [encode 'control])
    (jsexpr->string json #:null 'null #:encode encode)))

(: json->bytes
   (->* [JSON]
        [#:encode (U 'control 'all)]
        Bytes))
(define json->bytes
  (λ (json
      #:encode [encode 'control])
    (jsexpr->bytes json #:null 'null #:encode encode)))

(: read-json (-> Input-Port JSON))
(define (read-json port)
  (let ([x (read-json! port #:null 'null)])
    (cond
      [(eof-object? x) 'null]
      [else x])))

(: string->json (-> String JSON))
(define (string->json str)
  (string->jsexpr str #:null 'null))

(: bytes->json (-> Bytes JSON))
(define (bytes->json bytes)
  (bytes->jsexpr bytes #:null 'null))
