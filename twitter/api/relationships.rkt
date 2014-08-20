#lang typed/racket/base

(provide friends/list friends/list-ids friends/no-retweets
         followers/list followers/list-ids
         friendship/create friendship/show friendship/update friendship/destroy
         friendships/incoming friendships/outgoing
         friendships/lookup-by-id friendships/lookup-by-name)

(require racket/match
         (only-in racket/string
                  string-join))
(require "../oauth/oauth.rkt"
         "../types/json.rkt")
(require "../types/types.rkt"
         "../types/type-helpers.rkt")
(require "../helpers.rkt")
(require "private/api-helpers.rkt")
(require (only-in "../data/relationships.rkt"
                  json->users-relationship))

;; ---------------------------------------------------------------------------------------------------
;; Friends

(: friends/list
   (->* [Twitter-User*]
        [#:client Client #:token Token #:cursor (Perhaps Integer) #:count (Perhaps Integer)
                  #:skip-status? (Perhaps Boolean) #:include-entities? (Perhaps Boolean)]
        [U (Cursored Twitter-User) Twitter-Error]))
(define friends/list
  (λ (user
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:cursor [cursor (Nothing)]
      #:count [count (Nothing)]
      #:skip-status? [skip-status? (current-skip-status?)]
      #:include-entities? [include-entities? (current-include-entities?)])
    (define ~count (enforce-range count 1 200))
    (define ~cursor (enforce-range cursor -1 '+inf))
    (define response
      (oauth/get (twitter/url "/1.1/friends/list.json"
                              `(,(user*->param user)
                                ,@(perhaps-param "cursor" ~cursor number->string)
                                ,@(perhaps-param "count" ~count number->string)
                                ,@(perhaps-param "skip_status" skip-status? bool->string)
                                ,@(perhaps-param "include_entities" include-entities? bool->string)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(hash? x) (let ([prev (hash-ref (json-hash x) 'previous_cursor JSON-Null)]
                            [next (hash-ref (json-hash x) 'next_cursor JSON-Null)]
                            [vals (hash-ref (json-hash x) 'users JSON-Null)])
                        (cond
                          [(and (exact-integer? prev) (exact-integer? next) (list-of-hash? vals))
                           (Cursored (map (compose Twitter-User json-hash) vals)
                                     prev
                                     next)]
                          [else (response-error 'friends/list x)]))]
           [else (response-error 'friends/list x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: friends/list-ids
   (->* [Twitter-User*]
        [#:client Client #:token Token #:cursor (Perhaps Integer) #:count (Perhaps Integer)]
        [U (Cursored Twitter-UserID) Twitter-Error]))
(define friends/list-ids
  (λ (user
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:cursor [cursor (Nothing)]
      #:count [count (Nothing)])
    (define ~count (enforce-range count 1 200))
    (define ~cursor (enforce-range cursor -1 '+inf))
    (define response
      (oauth/get (twitter/url "/1.1/friends/ids.json"
                              `(,(user*->param user)
                                ,@(perhaps-param "cursor" ~cursor number->string)
                                ,@(perhaps-param "count" ~count number->string)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(hash? x) (let ([prev (hash-ref (json-hash x) 'previous_cursor JSON-Null)]
                            [next (hash-ref (json-hash x) 'next_cursor JSON-Null)]
                            [vals (hash-ref (json-hash x) 'ids JSON-Null)])
                        (cond
                          [(and (exact-integer? prev) (exact-integer? next) (list-of-integer? vals))
                           (Cursored (map Twitter-UserID vals)
                                     prev
                                     next)]
                          [else (response-error 'friends/list-ids x)]))]
           [else (response-error 'friends/list-ids x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: friends/no-retweets
   (->* []
        [#:client Client #:token Token]
        [U (Listof Twitter-UserID) Twitter-Error]))
(define friends/no-retweets
  (λ (#:client [client (current-client)] #:token [token (current-token)])
    (define response
      (oauth/get (twitter/url "/1.1/friendships/no_retweets/ids.json"
                              `(,(Param "stringify_ids" "false")))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(list-of-integer? x) (map Twitter-UserID x)]
           [else (response-error 'friends/no-retweets x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

;; ---------------------------------------------------------------------------------------------------
;; Followers

(: followers/list
   (->* [Twitter-User*]
        [#:client Client #:token Token #:cursor (Perhaps Integer) #:count (Perhaps Integer)
                  #:skip-status? (Perhaps Boolean) #:include-entities? (Perhaps Boolean)]
        [U (Cursored Twitter-User) Twitter-Error]))
(define followers/list
  (λ (user
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:cursor [cursor (Nothing)]
      #:count [count (Nothing)]
      #:skip-status? [skip-status? (current-skip-status?)]
      #:include-entities? [include-entities? (current-include-entities?)])
    (define ~count (enforce-range count 1 200))
    (define ~cursor (enforce-range cursor -1 '+inf))
    (define response
      (oauth/get (twitter/url "/1.1/followers/list.json"
                              `(,(user*->param user)
                                ,@(perhaps-param "cursor" ~cursor number->string)
                                ,@(perhaps-param "count" ~count number->string)
                                ,@(perhaps-param "skip_status" skip-status? bool->string)
                                ,@(perhaps-param "include_entities" include-entities? bool->string)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(hash? x) (let ([prev (hash-ref (json-hash x) 'previous_cursor JSON-Null)]
                            [next (hash-ref (json-hash x) 'next_cursor JSON-Null)]
                            [vals (hash-ref (json-hash x) 'users JSON-Null)])
                        (cond
                          [(and (exact-integer? prev) (exact-integer? next) (list-of-hash? vals))
                           (Cursored (map (compose Twitter-User json-hash) vals)
                                     prev
                                     next)]
                          [else (response-error 'followers/list x)]))]
           [else (response-error 'followers/list x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: followers/list-ids
   (->* [Twitter-User*]
        [#:client Client #:token Token #:cursor (Perhaps Integer) #:count (Perhaps Integer)]
        [U (Cursored Twitter-UserID) Twitter-Error]))
(define followers/list-ids
  (λ (user
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:cursor [cursor (Nothing)]
      #:count [count (Nothing)])
    (define ~count (enforce-range count 1 200))
    (define ~cursor (enforce-range cursor -1 '+inf))
    (define response
      (oauth/get (twitter/url "/1.1/followers/ids.json"
                              `(,(user*->param user)
                                ,@(perhaps-param "cursor" ~cursor number->string)
                                ,@(perhaps-param "count" ~count number->string)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(hash? x) (let ([prev (hash-ref (json-hash x) 'previous_cursor JSON-Null)]
                            [next (hash-ref (json-hash x) 'next_cursor JSON-Null)]
                            [vals (hash-ref (json-hash x) 'ids JSON-Null)])
                        (cond
                          [(and (exact-integer? prev) (exact-integer? next) (list-of-integer? vals))
                           (Cursored (map Twitter-UserID vals)
                                     prev
                                     next)]
                          [else (response-error 'followers/list-ids x)]))]
           [else (response-error 'followers/list-ids x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

;; ---------------------------------------------------------------------------------------------------
;; Friendships

(: friendship/create
   (->* [Twitter-User*]
        [#:client Client #:token Token #:follow? (Perhaps Boolean)]
        [U Twitter-User Twitter-Error]))
(define friendship/create
  (λ (user
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:follow? [follow? #t])
    (define response
      (oauth/post (twitter/url "/1.1/friendships/create.json")
                  client
                  token
                  #:data `(,(user*->param user)
                           ,@(perhaps-param "follow" follow? bool->string))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(hash? x) (Twitter-User (json-hash x))]
           [else (response-error 'friendship/create x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: friendship/show
   (->* [Twitter-User* Twitter-User*]
        [#:client Client #:token Token]
        [U Twitter-Users-Relationship Twitter-Error]))
(define friendship/show
  (λ (source
      target
      #:client [client (current-client)]
      #:token [token (current-token)])
    (define source_key (if (Twitter-Username? source) "source_screen_name" "source_id"))
    (define target_key (if (Twitter-Username? target) "target_screen_name" "target_id"))
    (define response
      (oauth/get (twitter/url "/1.1/friendships/show.json"
                              `(,(Param source_key (user*->string source))
                                ,(Param target_key (user*->string target))))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(hash? x) (json->users-relationship (json-hash x))]
           [else (response-error 'friendship/show x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: friendship/update
   (->* [Twitter-User*]
        [#:client Client #:token Token #:device? (Perhaps Boolean) #:retweets? (Perhaps Boolean)]
        [U Twitter-Users-Relationship Twitter-Error]))
(define friendship/update
  (λ (user
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:device? [device? (Nothing)]
      #:retweets? [retweets? (Nothing)])
    (define response
      (oauth/put (twitter/url "/1.1/friendships/update.json")
                 client
                 token
                 #:data `(,(user*->param user)
                          ,@(perhaps-param "device" device? bool->string)
                          ,@(perhaps-param "retweets" retweets? bool->string))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(hash? x) (json->users-relationship (json-hash x))]
           [else (response-error 'friendship/update x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: friendship/destroy
   (->* [Twitter-User*]
        [#:client Client #:token Token]
        [U Twitter-User Twitter-Error]))
(define friendship/destroy
  (λ (user
      #:client [client (current-client)]
      #:token [token (current-token)])
    (define response
      (oauth/post (twitter/url "/1.1/friendships/destroy.json")
                  client
                  token
                  #:data `(,(user*->param user))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(hash? x) (Twitter-User (json-hash x))]
           [else (response-error 'friendship/destroy x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: friendships/incoming
   (->* []
        [#:client Client #:token Token #:cursor (Perhaps Integer)]
        [U (Cursored Twitter-UserID) Twitter-Error]))
(define friendships/incoming
  (λ (#:client [client (current-client)] #:token [token (current-token)] #:cursor [cursor (Nothing)])
    (define ~cursor (enforce-range cursor -1 '+inf))
    (define response
      (oauth/get (twitter/url "/1.1/friendships/incoming.json"
                              `(,@(perhaps-param "cursor" ~cursor number->string)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(hash? x) (let ([prev (hash-ref (json-hash x) 'previous_cursor JSON-Null)]
                            [next (hash-ref (json-hash x) 'next_cursor JSON-Null)]
                            [vals (hash-ref (json-hash x) 'ids JSON-Null)])
                        (cond
                          [(and (exact-integer? prev) (exact-integer? next) (list-of-integer? vals))
                           (Cursored (map Twitter-UserID vals)
                                     prev
                                     next)]
                          [else (response-error 'friendships/incoming x)]))]
           [else (response-error 'friendships/incoming x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: friendships/outgoing
   (->* []
        [#:client Client #:token Token #:cursor (Perhaps Integer)]
        [U (Cursored Twitter-UserID) Twitter-Error]))
(define friendships/outgoing
  (λ (#:client [client (current-client)] #:token [token (current-token)] #:cursor [cursor (Nothing)])
    (define ~cursor (enforce-range cursor -1 '+inf))
    (define response
      (oauth/get (twitter/url "/1.1/friendships/outgoing.json"
                              `(,@(perhaps-param "cursor" ~cursor number->string)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(hash? x) (let ([prev (hash-ref (json-hash x) 'previous_cursor JSON-Null)]
                            [next (hash-ref (json-hash x) 'next_cursor JSON-Null)]
                            [vals (hash-ref (json-hash x) 'ids JSON-Null)])
                        (cond
                          [(and (exact-integer? prev) (exact-integer? next) (list-of-integer? vals))
                           (Cursored (map Twitter-UserID vals)
                                     prev
                                     next)]
                          [else (response-error 'friendships/outgoing x)]))]
           [else (response-error 'friendships/outgoing x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: friendships/lookup-by-id
   (->* [(Listof (U Twitter-UserID Twitter-User))]
        [#:client Client #:token Token]
        [U (Listof Twitter-User-Connexions) Twitter-Error]))
(define friendships/lookup-by-id
  (λ (users
      #:client [client (current-client)]
      #:token [token (current-token)])
    (define response
      (oauth/get (twitter/url "/1.1/friendships/lookup.json"
                              `(,(Param "user_id" (string-join (map user*->string users) ","))))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(list-of-hash? x) (map (compose Twitter-User-Connexions json-hash) x)]
           [else (response-error 'friendships/lookup-by-id x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: friendships/lookup-by-name
   (->* [(Listof Twitter-Username)]
        [#:client Client #:token Token]
        [U (Listof Twitter-User-Connexions) Twitter-Error]))
(define friendships/lookup-by-name
  (λ (users
      #:client [client (current-client)]
      #:token [token (current-token)])
    (define response
      (oauth/get (twitter/url "/1.1/friendships/lookup.json"
                              `(,(Param "screen_name" (string-join (map user*->string users) ","))))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(list-of-hash? x) (map (compose Twitter-User-Connexions json-hash) x)]
           [else (response-error 'friendships/lookup-by-name x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))