#lang typed/racket/base

(provide trends/place
         trends/available
         trends/closest)

(require racket/match)
(require "../oauth/oauth.rkt")
(require "../types/types.rkt"
         "../types/type-helpers.rkt"
         "../types/json.rkt")
(require "../helpers.rkt")
(require "private/api-helpers.rkt")

;; ---------------------------------------------------------------------------------------------------
;; Twitter-Trends

(: trends/place
   (->* [WOE*]
        [#:client Client #:token Token #:exclude (Perhaps 'hashtags)]
        [U (Listof Twitter-Trend) Twitter-Error]))
(define trends/place
  (λ (place
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:exclude [exclude (Nothing)])
    (define response
      (oauth/get (twitter/url "/1.1/trends/place.json"
                              `(,(woe*->param place)
                                ,@(perhaps-param "exclude" exclude symbol->string)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(list-of-hash? x) (let* ([y (match x [(list a) (json-hash a)])]
                                     [z (hash-ref y 'trends JSON-Null)])
                                (cond
                                  [(list-of-hash? z) (map (compose Twitter-Trend json-hash) z)]
                                  [else (response-error 'trends/place x)]))]
           [else (response-error 'trends/place x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: trends/available
   (->* []
        [#:client Client #:token Token]
        [U (Listof WOE-Location) Twitter-Error]))
(define trends/available
  (λ (#:client [client (current-client)] #:token [token (current-token)])
    (define response
      (oauth/get (twitter/url "/1.1/trends/available.json")
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(list-of-hash? x) (map (compose WOE-Location json-hash) x)]
           [else (response-error 'trends/available x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: trends/closest
   (->* [Coördinates]
        [#:client Client #:token Token]
        [U (Listof WOE-Location) Twitter-Error]))
(define trends/closest
  (λ (coördinates
      #:client [client (current-client)]
      #:token [token (current-token)])
    (define response
      (oauth/get (twitter/url "/1.1/trends/closest.json"
                              `(,@(coördinates->perhaps-params coördinates)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(list-of-hash? x) (map (compose WOE-Location json-hash) x)]
           [else (response-error 'trends/closest x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))
