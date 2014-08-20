#lang typed/racket/base

(provide (all-defined-out))

(struct Client
  ([key : String]
   [secret : String]))

(struct Token
  ([key : String]
   [secret : String]))

(struct OAuth-URL
  ([protocol : (U 'http 'https)]
   [host : String]
   [path : String]
   [query : HTTP-Parameter-List]))

(struct HTTP-Parameter
  ([key : String]
   [value : String]))

(define-type HTTP-Parameter-List (Listof HTTP-Parameter))

(: Param (->* [String] [String] HTTP-Parameter))
(define (Param key [value ""])
  (HTTP-Parameter key value))

(define-type Signing-Method (U 'HMAC-SHA1 'PLAINTEXT))
(define-type HTTP-Method (U 'GET 'POST 'PUT 'DELETE))

(struct HTTP-Response
  ([status : Bytes]
   [headers : (Listof Bytes)]
   [body : Input-Port]))
