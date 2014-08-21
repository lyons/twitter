#lang typed/racket/base

(provide list/create list/show list/update list/destroy
         list/statuses
         list/member/create list/member/show list/member/destroy
         list/members/create-by-id list/members/create-by-name
         list/members
         list/members/destroy-by-id list/members/destroy-by-name
         list/subscribe list/unsubscribe list/subscribers list/subscribed?
         lists/list lists/owned lists/subscribed
         lists/membership)

(require racket/match
         (only-in racket/string
                  string-join))
(require oauth/client)
(require "../types/types.rkt"
         "../types/type-helpers.rkt"
         "../types/json.rkt")
(require "../helpers.rkt")
(require "private/api-helpers.rkt")

;; ---------------------------------------------------------------------------------------------------
;; List CRUD

(: list/create
   (->* [String]
        [#:client Client #:token Token #:mode (Perhaps (U 'public 'private))
                  #:description (Perhaps String)]
        [U Twitter-List Twitter-Error]))
(define list/create
  (λ (name
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:mode [mode (Nothing)]
      #:description [description (Nothing)])
    (define response
      (oauth/post (twitter/url "/1.1/lists/create.json")
                  client
                  token
                  #:data `(,(Param "name" name)
                           ,@(perhaps-param "mode" mode symbol->string)
                           ,@(perhaps-param "description" description string-identity))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(hash? x) (Twitter-List (json-hash x))]
           [else (response-error 'list/create x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: list/show
   (->* [Twitter-List*]
        [#:client Client #:token Token]
        [U Twitter-List Twitter-Error]))
(define list/show
  (λ (list
      #:client [client (current-client)]
      #:token [token (current-token)])
    (define response
      (oauth/get (twitter/url "/1.1/lists/show.json"
                              `(,@(tw-list*->params list)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(hash? x) (Twitter-List (json-hash x))]
           [else (response-error 'list/show x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: list/update
   (->* [Twitter-List*]
        [#:client Client #:token Token #:name (Perhaps String) #:mode (Perhaps (U 'public 'private))
                  #:description (Perhaps String)]
        [U Twitter-List Twitter-Error]))
(define list/update
  (λ (list
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:name [name (Nothing)]
      #:mode [mode (Nothing)]
      #:description [description (Nothing)])
    (define response
      (oauth/post (twitter/url "/1.1/lists/update.json")
                  client
                  token
                  #:data `(,@(tw-list*->params list)
                           ,@(perhaps-param "name" name string-identity)
                           ,@(perhaps-param "mode" mode symbol->string)
                           ,@(perhaps-param "description" description string-identity))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(hash? x) (Twitter-List (json-hash x))]
           [else (response-error 'list/update x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: list/destroy
   (->* [Twitter-List*]
        [#:client Client #:token Token]
        [U Twitter-List Twitter-Error]))
(define list/destroy
  (λ (list
      #:client [client (current-client)]
      #:token [token (current-token)])
    (define response
      (oauth/post (twitter/url "/1.1/lists/destroy.json")
                  client
                  token
                  #:data `(,@(tw-list*->params list))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(hash? x) (Twitter-List (json-hash x))]
           [else (response-error 'list/destroy x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

;; ---------------------------------------------------------------------------------------------------
;; List timelines

(: list/statuses
   (->* [Twitter-List*]
        [#:client Client #:token Token #:count (Perhaps Integer) #:since-id (Perhaps TweetID)
                  #:max-id (Perhaps TweetID) #:include-entities? (Perhaps Boolean)
                  #:include-retweets? (Perhaps Boolean)]
        [U (Listof Tweet) Twitter-Error]))
(define list/statuses
  (λ (list
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:count [count (Nothing)]
      #:since-id [since-id (Nothing)]
      #:max-id [max-id (Nothing)]
      #:include-entities? [include-entities? (current-include-entities?)]
      #:include-retweets? [include-retweets? (Nothing)])
    (define ~count (enforce-range count 1 200))
    (define response
      (oauth/get (twitter/url "/1.1/lists/statuses.json"
                              `(,@(tw-list*->params list)
                                ,@(perhaps-param "count" ~count number->string)
                                ,@(perhaps-param "since_id" since-id tweetid->string)
                                ,@(perhaps-param "max_id" max-id tweetid->string)
                                ,@(perhaps-param "include_entities" include-entities? bool->string)
                                ,@(perhaps-param "include_rts" include-retweets? bool->string)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(list-of-hash? x) (map (compose Tweet json-hash) x)]
           [else (response-error 'list/statuses x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

;; ---------------------------------------------------------------------------------------------------
;; List members CRUD

(: list/member/create
   (->* [Twitter-List* Twitter-User*]
        [#:client Client #:token Token]
        [U Twitter-User Twitter-Error]))
(define list/member/create
  (λ (list
      user
      #:client [client (current-client)]
      #:token [token (current-token)])
    (define response
      (oauth/post (twitter/url "/1.1/lists/members/create.json")
                  client
                  token
                  #:data `(,@(tw-list*->params list)
                           ,(user*->param user))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(hash? x) (Twitter-User (json-hash x))]
           [else (response-error 'list/member/create x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: list/member/show
   (->* [Twitter-List* Twitter-User*]
        [#:client Client #:token Token #:include-entities? (Perhaps Boolean)
                  #:skip-status? (Perhaps Boolean)]
        [U Twitter-User Twitter-Error]))
(define list/member/show
  (λ (list
      user
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:include-entities? [include-entities? (current-include-entities?)]
      #:skip-status? [skip-status? (current-skip-status?)])
    (define response
      (oauth/get (twitter/url "/1.1/lists/members/show.json"
                              `(,@(tw-list*->params list)
                                ,(user*->param user)
                                ,@(perhaps-param "include_entities" include-entities? bool->string)
                                ,@(perhaps-param "skip_status" skip-status? bool->string)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(hash? x) (Twitter-User (json-hash x))]
           [else (response-error 'list/member/show x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: list/member/destroy
   (->* [Twitter-List* Twitter-User*]
        [#:client Client #:token Token]
        [U 'success Twitter-Error]))
(define list/member/destroy
  (λ (list
      user
      #:client [client (current-client)]
      #:token [token (current-token)])
    (define response
      (oauth/post (twitter/url "/1.1/lists/members/destroy.json")
                  client
                  token
                  #:data `(,@(tw-list*->params list)
                           ,(user*->param user))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ _) 'success]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: list/members/create-by-id
     (->* [Twitter-List* (Listof (U Twitter-UserID Twitter-User))]
          [#:client Client #:token Token]
          [U 'success Twitter-Error]))
(define list/members/create-by-id
  (λ (list
      user
      #:client [client (current-client)]
      #:token [token (current-token)])
    (define response
      (oauth/post (twitter/url "/1.1/lists/members/create_all.json")
                  client
                  token
                  #:data `(,@(tw-list*->params list)
                           ,(Param "user_id" (string-join (map user*->string user) ",")))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ _) 'success]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: list/members/create-by-name
     (->* [Twitter-List* (Listof Twitter-Username)]
          [#:client Client #:token Token]
          [U 'success Twitter-Error]))
(define list/members/create-by-name
  (λ (list
      user
      #:client [client (current-client)]
      #:token [token (current-token)])
    (define response
      (oauth/post (twitter/url "/1.1/lists/members/create_all.json")
                  client
                  token
                  #:data `(,@(tw-list*->params list)
                           ,(Param "screen_name" (string-join (map user*->string user) ",")))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ _) 'success]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: list/members
   (->* [Twitter-List*]
        [#:client Client #:token Token #:cursor (Perhaps Integer)
                  #:include-entities? (Perhaps Boolean) #:skip-status? (Perhaps Boolean)]
        [U (Cursored Twitter-User) Twitter-Error]))
(define list/members
  (λ (list
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:cursor [cursor (Nothing)]
      #:include-entities? [include-entities? (current-include-entities?)]
      #:skip-status? [skip-status? (current-skip-status?)])
    (define ~cursor (enforce-range cursor -1 '+inf))
    (define response
      (oauth/get (twitter/url "/1.1/lists/members.json"
                              `(,@(tw-list*->params list)
                                ,@(perhaps-param "cursor" ~cursor number->string)
                                ,@(perhaps-param "include_entities" include-entities? bool->string)
                                ,@(perhaps-param "skip_status" skip-status? bool->string)))
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
                          [else (response-error 'list/members x)]))]
           [else (response-error 'list/members x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: list/members/destroy-by-id
   (->* [Twitter-List* (Listof (U Twitter-UserID Twitter-User))]
        [#:client Client #:token Token]
        [U 'success Twitter-Error]))
(define list/members/destroy-by-id
  (λ (list
      user
      #:client [client (current-client)]
      #:token [token (current-token)])
    (define response
      (oauth/post (twitter/url "/1.1/lists/members/destroy_all.json")
                  client
                  token
                  #:data `(,@(tw-list*->params list)
                           ,(Param "user_id" (string-join (map user*->string user) ",")))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ _) 'success]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: list/members/destroy-by-name
   (->* [Twitter-List* (Listof Twitter-Username)]
        [#:client Client #:token Token]
        [U 'success Twitter-Error]))
(define list/members/destroy-by-name
  (λ (list
      user
      #:client [client (current-client)]
      #:token [token (current-token)])
    (define response
      (oauth/post (twitter/url "/1.1/lists/members/destroy_all.json")
                  client
                  token
                  #:data `(,@(tw-list*->params list)
                           ,(Param "screen_name" (string-join (map user*->string user) ",")))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ _) 'success]
      [(HTTP-Response status _ body) (twitter/error status body)])))

;; ---------------------------------------------------------------------------------------------------
;; Subscriptions

(: list/subscribe
   (->* [Twitter-List*]
        [#:client Client #:token Token]
        [U Twitter-User Twitter-Error]))
(define list/subscribe
  (λ (list
      #:client [client (current-client)]
      #:token [token (current-token)])
    (define response
      (oauth/post (twitter/url "/1.1/lists/subscribers/create.json")
                  client
                  token
                  #:data `(,@(tw-list*->params list))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(hash? x) (Twitter-User (json-hash x))]
           [else (response-error 'list/subscribe x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: list/unsubscribe
   (->* [Twitter-List*]
        [#:client Client #:token Token]
        [U 'success Twitter-Error]))
(define list/unsubscribe
  (λ (list
      #:client [client (current-client)]
      #:token [token (current-token)])
    (define response
      (oauth/post (twitter/url "/1.1/lists/subscribers/destroy.json")
                  client
                  token
                  #:data `(,@(tw-list*->params list))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ _) 'success]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: list/subscribers
   (->* [Twitter-List*]
        [#:client Client #:token Token #:cursor (Perhaps Integer)
                  #:include-entities? (Perhaps Boolean) #:skip-status? (Perhaps Boolean)]
        [U (Cursored Twitter-User) Twitter-Error]))
(define list/subscribers
  (λ (list
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:cursor [cursor (Nothing)]
      #:include-entities? [include-entities? (current-include-entities?)]
      #:skip-status? [skip-status? (current-skip-status?)])
    (define ~cursor (enforce-range cursor -1 '+inf))
    (define response
      (oauth/get (twitter/url "/1.1/lists/subscribers.json"
                              `(,@(tw-list*->params list)
                                ,@(perhaps-param "cursor" ~cursor number->string)
                                ,@(perhaps-param "include_entities" include-entities? bool->string)
                                ,@(perhaps-param "skip_status" skip-status? bool->string)))
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
                          [else (response-error 'list/subscribers x)]))]
           [else (response-error 'list/subscribers x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: list/subscribed?
   (->* [Twitter-List* Twitter-User*]
        [#:client Client #:token Token #:include-entities? (Perhaps Boolean)
                  #:skip-status? (Perhaps Boolean)]
        [U Twitter-User Twitter-Error]))
(define list/subscribed?
  (λ (list
      user
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:include-entities? [include-entities? (current-include-entities?)]
      #:skip-status? [skip-status? (current-skip-status?)])
    (define response
      (oauth/get (twitter/url "/1.1/lists/subscribers/show.json"
                              `(,@(tw-list*->params list)
                                ,(user*->param user)
                                ,@(perhaps-param "include_entities" include-entities? bool->string)
                                ,@(perhaps-param "skip_status" skip-status? bool->string)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(hash? x) (Twitter-User (json-hash x))]
           [else (response-error 'list/subscribed? x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

;; ---------------------------------------------------------------------------------------------------
;; User ownership and subscription

(: lists/list
   (->* []
        [#:client Client #:token Token #:user (Perhaps Twitter-User*) #:reverse? (Perhaps Boolean)]
        [U (Listof Twitter-List) Twitter-Error]))
(define lists/list
  (λ (#:client [client (current-client)] #:token [token (current-token)]
               #:user [user (Nothing)]
               #:reverse? [reverse? (Nothing)])
    (define response
      (oauth/get (twitter/url "/1.1/lists/list.json"
                              `(,@(user*->spliceable-param user)
                                ,@(perhaps-param "reverse" reverse? bool->string)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(list-of-hash? x) (map (compose Twitter-List json-hash) x)]
           [else (response-error 'lists/list x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: lists/owned
   (->* [Twitter-User*]
        [#:client Client #:token Token #:count (Perhaps Integer) #:cursor (Perhaps Integer)]
        [U (Cursored Twitter-List) Twitter-Error]))
(define lists/owned
  (λ (user
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:count [count (Nothing)]
      #:cursor [cursor (Nothing)])
    (define ~count (enforce-range count 1 1000))
    (define ~cursor (enforce-range cursor -1 '+inf))
    (define response
      (oauth/get (twitter/url "/1.1/lists/ownerships.json"
                              `(,(user*->param user)
                                ,@(perhaps-param "count" ~count number->string)
                                ,@(perhaps-param "cursor" ~cursor number->string)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(hash? x) (let ([prev (hash-ref (json-hash x) 'previous_cursor JSON-Null)]
                            [next (hash-ref (json-hash x) 'next_cursor JSON-Null)]
                            [vals (hash-ref (json-hash x) 'lists JSON-Null)])
                        (cond
                          [(and (exact-integer? prev) (exact-integer? next) (list-of-hash? vals))
                           (Cursored (map (compose Twitter-List json-hash) vals)
                                     prev
                                     next)]
                          [else (response-error 'list/owned x)]))]
           [else (response-error 'list/owned x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: lists/subscribed
   (->* [Twitter-User*]
        [#:client Client #:token Token #:count (Perhaps Integer) #:cursor (Perhaps Integer)]
        [U (Cursored Twitter-List) Twitter-Error]))
(define lists/subscribed
  (λ (user
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:count [count (Nothing)]
      #:cursor [cursor (Nothing)])
    (define ~count (enforce-range count 1 1000))
    (define ~cursor (enforce-range cursor -1 '+inf))
    (define response
      (oauth/get (twitter/url "/1.1/lists/subscriptions.json"
                              `(,(user*->param user)
                               ,@(perhaps-param "count" ~count number->string)
                               ,@(perhaps-param "cursor" ~cursor number->string)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(hash? x) (let ([prev (hash-ref (json-hash x) 'previous_cursor JSON-Null)]
                            [next (hash-ref (json-hash x) 'next_cursor JSON-Null)]
                            [vals (hash-ref (json-hash x) 'lists JSON-Null)])
                        (cond
                          [(and (exact-integer? prev) (exact-integer? next) (list-of-hash? vals))
                           (Cursored (map (compose Twitter-List json-hash) vals)
                                     prev
                                     next)]
                          [else (response-error 'lists/subscribed x)]))]
           [else (response-error 'lists/subscribed x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

;; ---------------------------------------------------------------------------------------------------
;; User membership

(: lists/membership
   (->* []
        [#:client Client #:token Token #:user (Perhaps Twitter-User*) #:count (Perhaps Integer)
                  #:cursor (Perhaps Integer) #:filter-to-owned-lists? (Perhaps Boolean)]
        [U (Cursored Twitter-List) Twitter-Error]))
(define lists/membership
  (λ (#:client [client (current-client)] #:token [token (current-token)]
               #:user [user (Nothing)]
               #:count [count (Nothing)]
               #:cursor [cursor (Nothing)]
               #:filter-to-owned-lists? [filter? (Nothing)])
    (define ~count (enforce-range count 1 1000))
    (define ~cursor (enforce-range cursor -1 '+inf))
    (define response
      (oauth/get (twitter/url "/1.1/lists/memberships.json"
                              `(,@(user*->spliceable-param user)
                                ,@(perhaps-param "count" ~count number->string)
                                ,@(perhaps-param "cursor" ~cursor number->string)
                                ,@(perhaps-param "filter_to_owned_lists" filter? bool->string)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(hash? x) (let ([prev (hash-ref (json-hash x) 'previous_cursor JSON-Null)]
                            [next (hash-ref (json-hash x) 'next_cursor JSON-Null)]
                            [vals (hash-ref (json-hash x) 'lists JSON-Null)])
                        (cond
                          [(and (exact-integer? prev) (exact-integer? next) (list-of-hash? vals))
                           (Cursored (map (compose Twitter-List json-hash) vals)
                                     prev
                                     next)]
                          [else (response-error 'lists/membership x)]))]
           [else (response-error 'lists/membership x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))
