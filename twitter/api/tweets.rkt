#lang typed/racket/base

(provide status/create status/create-with-media status/show status/destroy
         status/retweet status/retweets status/retweeters
         statuses/lookup
         status/oembed
         favourite/create favourite/destroy favourites/list)

(require racket/match
         (only-in racket/path
                  filename-extension)
         (only-in racket/string
                  string-join))
(require (only-in typed/net/url
                  [url URL]
                  url->string))
(require oauth/client)
(require "../types/types.rkt"
         "../types/type-helpers.rkt"
         "../types/json.rkt")
(require "../helpers.rkt")
(require "private/api-helpers.rkt")

;; ---------------------------------------------------------------------------------------------------
;; Tweets

(: status/create
   (->* [String]
        [#:client Client #:token Token #:in-reply-to (Perhaps TweetID) #:sensitive? (Perhaps Boolean)
                  #:coördinates (Perhaps Coördinates) #:display-coördinates? (Perhaps Boolean)
                  #:place-id (Perhaps Twitter-PlaceID) #:trim-user? (Perhaps Boolean)]
        [U Tweet Twitter-Error]))
(define status/create
  (λ (status
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:in-reply-to [in-reply-to (Nothing)]
      #:sensitive? [sensitive? (Nothing)]
      #:coördinates [coördinates (Nothing)]
      #:display-coördinates? [display-coördinates? (Nothing)]
      #:place-id [place-id (Nothing)]
      #:trim-user? [trim-user? (current-trim-user?)])
    ; Should enforce length of tweet
    (define response
      (oauth/post (twitter/url "/1.1/statuses/update.json")
                  client
                  token
                  #:data `(,(Param "status" status)
                           ,@(perhaps-param "in_reply_to_status_id" in-reply-to tweetid->string)
                           ,@(perhaps-param "possibly_sensitive" sensitive? bool->string)
                           ,@(coördinates->perhaps-params coördinates)
                           ,@(perhaps-param "display_coordinates" display-coördinates? bool->string)
                           ,@(perhaps-param "place_id" place-id placeid->string)
                           ,@(perhaps-param "trim_user" trim-user? bool->string))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(hash? x) (Tweet (json-hash x))]
           [else (response-error 'status/create x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: status/create-with-media
   (->* [String Path]
        [#:client Client #:token Token #:in-reply-to (Perhaps TweetID)
                  #:coördinates (Perhaps Coördinates) #:display-coördinates? (Perhaps Boolean)
                  #:sensitive? (Perhaps Boolean) #:place-id (Perhaps Twitter-PlaceID)]
        [U Tweet Twitter-Error]))
(define status/create-with-media
  (λ (status
      path
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:in-reply-to [in-reply-to (Nothing)]
      #:sensitive? [sensitive? #f] ; Maybe parameterise this
      #:coördinates [coördinates (Nothing)]
      #:display-coördinates? [display-coördinates? (Nothing)]
      #:place-id [place-id (Nothing)])
    ; Should enforce length of tweet
    (assert path file-exists?)
    (assert (filename-extension path) valid-image-format?)
    (assert (file-size path) (below-filesize-in-kb? 3072))
    (define response
      (oauth/post-multipart-data
       (twitter/url "/1.1/statuses/update_with_media.json")
       client
       token
       #:data `(,(Param "status" status)
                ,@(perhaps-param "in_reply_to_status_id" in-reply-to tweetid->string)
                ,@(perhaps-param "possibly_sensitive" sensitive? bool->string)
                ,@(coördinates->perhaps-params coördinates)
                ,@(perhaps-param "display_coordinates" display-coördinates? bool->string)
                ,@(perhaps-param "place_id" place-id placeid->string))
       #:files `(,path)))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(hash? x) (Tweet (json-hash x))]
           [else (response-error 'status/create-with-media x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: status/show
   (->* [TweetID]
        [#:client Client #:token Token #:trim-user? (Perhaps Boolean)
                  #:include-my-retweet? (Perhaps Boolean) #:include-entities? (Perhaps Boolean)]
        [U Tweet Twitter-Error]))
(define status/show
  (λ (id
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:trim-user? [trim-user? (current-trim-user?)]
      #:include-my-retweet? [include-rt? (Nothing)]
      #:include-entities? [include-entities? (current-include-entities?)])
    (define response
      (oauth/get (twitter/url "/1.1/statuses/show.json"
                              `(,(Param "id" (tweetid->string id))
                                ,@(perhaps-param "trim_user" trim-user? bool->string)
                                ,@(perhaps-param "include_my_retweet" include-rt? bool->string)
                                ,@(perhaps-param "include_entities" include-entities? bool->string)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(hash? x) (Tweet (json-hash x))]
           [else (response-error 'status/show x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: status/destroy
   (->* [TweetID]
        [#:client Client #:token Token #:trim-user? (Perhaps Boolean)]
        [U Tweet Twitter-Error]))
(define status/destroy
  (λ (id
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:trim-user? [trim-user? (current-trim-user?)])
    (define response
      (oauth/post (twitter/url (string-append "/1.1/statuses/destroy/"
                                             (tweetid->string id)
                                             ".json"))
                  client
                  token
                  #:data `(,@(perhaps-param "trim_user" trim-user? bool->string))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(hash? x) (Tweet (json-hash x))]
           [else (response-error 'status/destroy x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: status/retweet
   (->* [TweetID]
        [#:client Client #:token Token #:trim-user? (Perhaps Boolean)]
        [U Tweet Twitter-Error]))
(define status/retweet
  (λ (id
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:trim-user? [trim-user? (current-trim-user?)])
    (define response
      (oauth/post (twitter/url (string-append "/1.1/statuses/retweet/"
                                             (tweetid->string id)
                                             ".json"))
                  client
                  token
                  #:data `(,@(perhaps-param "trim_user" trim-user? bool->string))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(hash? x) (Tweet (json-hash x))]
           [else (response-error 'status/retweet x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: status/retweets
   (->* [TweetID]
        [#:client Client #:token Token #:count (Perhaps Integer)
                  #:trim-user? (Perhaps Boolean)]
        [U (Listof Tweet) Twitter-Error]))
(define status/retweets
  (λ (id
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:count [count (Nothing)]
      #:trim-user? [trim-user? (current-trim-user?)])
    (define ~count (enforce-range count 1 100))
    (define response
      (oauth/get (twitter/url (string-append "/1.1/statuses/retweets/"
                                             (tweetid->string id)
                                             ".json")
                              `(,@(perhaps-param "count" ~count number->string)
                                ,@(perhaps-param "trim_user" trim-user? bool->string)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(list-of-hash? x) (map (compose Tweet json-hash) x)]
           [else (response-error 'status/retweets x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: status/retweeters
   (->* [TweetID]
        [#:client Client #:token Token]
        [U (Listof Twitter-UserID) Twitter-Error]))
(define status/retweeters
  (λ (id
      #:client [client (current-client)]
      #:token [token (current-token)])
    (define response
      (oauth/get (twitter/url "/1.1/statuses/retweeters/ids.json")
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(hash? x) (let ([y (hash-ref x 'ids JSON-Null)])
                        (cond
                          [(list-of-integer? y) (map Twitter-UserID y)]
                          [else (response-error 'status/retweeters x)]))]
           [else (response-error 'status/retweeters x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: statuses/lookup
   (->* [(Listof TweetID)]
        [#:client Client #:token Token #:include-entities? (Perhaps Boolean)
                  #:trim-user? (Perhaps Boolean)]
        [U (Listof Tweet) Twitter-Error]))
(define statuses/lookup
  (λ (ids
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:include-entities? [include-entities? (current-include-entities?)]
      #:trim-user? [trim-user? (current-trim-user?)])
    (define response
      (oauth/post (twitter/url "/1.1/statuses/lookup.json")
                  client
                  token
                  #:data `(,(Param "id" (string-join (map tweetid->string ids) ","))
                           ,@(perhaps-param "include_entities" include-entities? bool->string)
                           ,@(perhaps-param "trim_user" trim-user? bool->string)
                           ,(Param "map" "false"))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(list-of-hash? x) (map (compose Tweet json-hash) x)]
           [else (response-error 'statuses/lookup x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: status/oembed
   (->* [TweetID URL]
        [#:client Client #:token Token #:max-width (Perhaps Positive-Integer)
                  #:hide-media? (Perhaps Boolean) #:hide-thread? (Perhaps Boolean)
                  #:omit-script? (Perhaps Boolean) #:align (Perhaps (U 'left 'right 'centre 'none))
                  #:related (Perhaps (Listof Twitter-Username))
                  #:lang (Perhaps Twitter-Language-Code)]
        [U (HashTable Symbol JSON) Twitter-Error]))
(define status/oembed
  (λ (id
      url
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:max-width [max-width (Nothing)]
      #:hide-media? [hide-media? (Nothing)]
      #:hide-thread? [hide-thread? (Nothing)]
      #:omit-script? [omit-script? (Nothing)]
      #:align [align (Nothing)]
      #:related [related (Nothing)]
      #:lang [lang (Nothing)])
    (define align~ (if (eq? align 'centre) 'center align))
    (define response
      (oauth/get (twitter/url "/1.1/statuses/oembed.json"
                              `(,(Param "id" (tweetid->string id))
                                ,(Param "url" (url->string url))
                                ,@(perhaps-param "maxwidth" max-width number->string)
                                ,@(perhaps-param "hide_media" hide-media? bool->string)
                                ,@(perhaps-param "hide_thread" hide-thread? bool->string)
                                ,@(perhaps-param "omit_script" omit-script? bool->string)
                                ,@(perhaps-param "align" align~ symbol->string)
                                ,@(perhaps-param "related"
                                                    related
                                                    (λ ([ls : (Listof Twitter-Username)])
                                                      (string-join (map user*->string ls) ",")))
                                ,@(perhaps-param "lang" lang symbol->string)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(hash? x)(json-hash x)]
           [else (response-error 'status/oembed x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

;; ---------------------------------------------------------------------------------------------------
;; Favourites

(: favourite/create
   (->* [TweetID]
        [#:client Client #:token Token #:include-entities? (Perhaps Boolean)]
        [U Tweet Twitter-Error]))
(define favourite/create
  (λ (id
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:include-entities? [include-entities? (current-include-entities?)])
    (define response
      (oauth/post (twitter/url "/1.1/favorites/create.json")
                  client
                  token
                  #:data `(,(Param "id" (tweetid->string id))
                           ,@(perhaps-param "include_entities" include-entities? bool->string))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(hash? x) (Tweet (json-hash x))]
           [else (response-error 'favourite/create x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: favourite/destroy
   (->* [TweetID]
        [#:client Client #:token Token #:include-entities? (Perhaps Boolean)]
        [U Tweet Twitter-Error]))
(define favourite/destroy
  (λ (id
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:include-entities? [include-entities? (current-include-entities?)])
    (define response
      (oauth/post (twitter/url "/1.1/favorites/destroy.json")
                  client
                  token
                  #:data `(,(Param "id" (tweetid->string id))
                           ,@(perhaps-param "include_entities" include-entities? bool->string))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(hash? x) (Tweet (json-hash x))]
           [else (response-error 'favourite/destroy x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: favourites/list
   (->* []
        [(Perhaps Twitter-User*) #:client Client #:token Token #:count (Perhaps Integer)
                         #:since-id (Perhaps TweetID) #:max-id (Perhaps TweetID)
                         #:include-entities? (Perhaps Boolean)]
        [U (Listof Tweet) Twitter-Error]))
(define favourites/list
  (λ ([user (Nothing)]
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:count [count (Nothing)]
      #:since-id [since-id (Nothing)]
      #:max-id [max-id (Nothing)]
      #:include-entities? [include-entities? (current-include-entities?)])
    (define ~count (enforce-range count 1 200))
    (define user-param-key (if (Twitter-Username? user) "screen_name" "user_id"))
    (define response
      (oauth/get (twitter/url "/1.1/favorites/list.json"
                              `(,@(perhaps-param user-param-key user user*->string)
                                ,@(perhaps-param "count" ~count number->string)
                                ,@(perhaps-param "since_id" since-id tweetid->string)
                                ,@(perhaps-param "max_id" max-id tweetid->string)
                                ,@(perhaps-param "include_entities" include-entities? bool->string)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(list-of-hash? x) (map (compose Tweet json-hash) x)]
           [else (response-error 'favourites/list x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))
