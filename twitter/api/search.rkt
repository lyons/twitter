#lang typed/racket/base

(provide search/tweets
         search/users)

(require racket/match)
(require "../oauth/oauth.rkt")
(require "../types/types.rkt"
         "../types/type-helpers.rkt"
         "../types/json.rkt")
(require "../helpers.rkt")
(require "private/api-helpers.rkt")

;; ---------------------------------------------------------------------------------------------------
;; Search

(define-type ISO-8601 String)

(: search/tweets
   (->* [String]
        [#:client Client #:token Token #:coördinates (Perhaps Coördinates) #:radius Natural
                  #:lang (Perhaps Twitter-Language-Code) #:locale (Perhaps 'ja)
                  #:result-type (Perhaps (U 'mixed 'recent 'popular))  #:count (Perhaps Integer)
                  #:until (Perhaps ISO-8601) #:since-id (Perhaps TweetID) #:max-id (Perhaps TweetID)
                  #:include-entities? (Perhaps Boolean)]
        [U (Listof Tweet) Twitter-Error]))
(define search/tweets
  (λ (query
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:coördinates [coördinates (Nothing)]
      #:radius [radius 10]
      #:lang [lang 'en]
      #:locale [locale (Nothing)]
      #:result-type [result-type (Nothing)]
      #:count [count (Nothing)]
      #:until [until (Nothing)]
      #:since-id [since-id (Nothing)]
      #:max-id [max-id (Nothing)]
      #:include-entities? [include-entities? (current-include-entities?)])
    (define geocode
      (match coördinates
        [(Nothing) '()]
        [(Coördinates lat long) `(,(Param "geocode"
                                          (string-append (number->string lat) ","
                                                         (number->string long) ","
                                                         (number->string radius) "km")))]))
    (define ~count (enforce-range count 1 100))
    (define response
      (oauth/get (twitter/url "/1.1/search/tweets.json"
                              `(,(Param "q" query)
                                ,@geocode
                                ,@(perhaps-param "lang" lang symbol->string)
                                ,@(perhaps-param "locale" locale symbol->string)
                                ,@(perhaps-param "result_type" result-type symbol->string)
                                ,@(perhaps-param "count" ~count number->string)
                                ,@(perhaps-param "until" until string-identity)
                                ,@(perhaps-param "since_id" since-id tweetid->string)
                                ,@(perhaps-param "max_id" max-id tweetid->string)
                                ,@(perhaps-param "include_entities" include-entities? bool->string)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body) ;; This horrible blob discards search metadata
       (define x (read-json body))
       (assert x hash?)
       (define y (hash-ref (json-hash x) 'statuses JSON-Null))
       (assert y list-of-hash?)
       (map (compose Tweet json-hash) y)]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: search/users
   (->* [String]
        [#:client Client #:token Token #:page (Perhaps Integer) #:count (Perhaps Integer)
                  #:include-entities? (Perhaps Boolean)]
        [U (Listof Twitter-User) Twitter-Error]))
(define search/users
  (λ (query
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:page [page (Nothing)]
      #:count [count (Nothing)] ; [1, 20]
      #:include-entities? [include-entities? (current-include-entities?)])
    (define ~page (enforce-range page 1 '+inf))
    (define ~count (enforce-range count 1 20))
    (define response
      (oauth/get (twitter/url "/1.1/users/search.json"
                              `(,(Param "q" query)
                                ,@(perhaps-param "page" ~page number->string)
                                ,@(perhaps-param "count" ~count number->string)
                                ,@(perhaps-param "include_entities" include-entities? bool->string)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(list-of-hash? x) (map (compose Twitter-User json-hash) x)]
           [else (response-error 'search/users x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))