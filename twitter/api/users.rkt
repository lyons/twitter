#lang typed/racket/base

(provide user/show users/lookup-by-id users/lookup-by-name
         user/contributors user/contributees
         user/profile-banner
         user/report-spam
         block/create block/destroy blocks/list blocks/list-ids
         mute/create mute/destroy mutes/list mutes/list-ids)

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
;; Users

(: user/show
   (->* [(U Twitter-UserID Twitter-Username)]
        [#:client Client #:token Token #:include-entities? (Perhaps Boolean)]
        [U Twitter-User Twitter-Error]))
(define user/show
  (λ (user
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:include-entities? [include-entities? (current-include-entities?)])
    (define response
      (oauth/get (twitter/url "/1.1/users/show.json"
                              `(,(user*->param user)
                                ,@(perhaps-param "include_entities" include-entities? bool->string)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(hash? x) (Twitter-User (json-hash x))]
           [else (response-error 'user/show x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: users/lookup-by-id
   (->* [(Listof Twitter-UserID)]
        [#:client Client #:token Token #:include-entities? (Perhaps Boolean)]
        [U (Listof Twitter-User) Twitter-Error]))
(define users/lookup-by-id
  (λ (ids
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:include-entities? [include-entities? (current-include-entities?)])
    (define response
      (oauth/post (twitter/url "/1.1/users/lookup.json")
                  client
                  token
                  #:data `(,(Param "user_id" (string-join (map user*->string ids) ","))
                           ,@(perhaps-param "include_entities" include-entities? bool->string))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(list-of-hash? x) (map (compose Twitter-User json-hash) x)]
           [else (response-error 'users/lookup-by-id x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: users/lookup-by-name
   (->* [(Listof Twitter-Username)]
        [#:client Client #:token Token #:include-entities? (Perhaps Boolean)]
        [U (Listof Twitter-User) Twitter-Error]))
(define users/lookup-by-name
  (λ (names
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:include-entities? [include-entities? (current-include-entities?)])
    (define response
      (oauth/post (twitter/url "/1.1/users/lookup.json")
                  client
                  token
                  #:data `(,(Param "screen_name" (string-join (map user*->string names) ","))
                           ,@(perhaps-param "include_entities" include-entities? bool->string))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(list-of-hash? x) (map (compose Twitter-User json-hash) x)]
           [else (response-error 'users/lookup-by-name x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: user/contributors
   (->* [Twitter-User*]
        [#:client Client #:token Token #:include-entities? (Perhaps Boolean)
                  #:skip-status? (Perhaps Boolean)]
        [U (Listof Twitter-User) Twitter-Error]))
(define user/contributors
  (λ (user
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:include-entities? [include-entities? (current-include-entities?)]
      #:skip-status? [skip-status? (current-skip-status?)])
    (define response
      (oauth/get (twitter/url "/1.1/users/contributors.json"
                              `(,(user*->param user)
                                ,@(perhaps-param "include_entities" include-entities? bool->string)
                                ,@(perhaps-param "skip_status" skip-status? bool->string)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(list-of-hash? x) (map (compose Twitter-User json-hash) x)]
           [else (response-error 'user/contributors x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: user/contributees
   (->* [Twitter-User*]
        [#:client Client #:token Token #:include-entities? (Perhaps Boolean)
                  #:skip-status? (Perhaps Boolean)]
        [U (Listof Twitter-User) Twitter-Error]))
(define user/contributees
  (λ (user
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:include-entities? [include-entities? (current-include-entities?)]
      #:skip-status? [skip-status? (current-skip-status?)])
    (define response
      (oauth/get (twitter/url "/1.1/users/contributees.json"
                              `(,(user*->param user)
                                ,@(perhaps-param "include_entities" include-entities? bool->string)
                                ,@(perhaps-param "skip_status" skip-status? bool->string)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(list-of-hash? x) (map (compose Twitter-User json-hash) x)]
           [else (response-error 'user/contributees x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: user/profile-banner
   (->* [Twitter-User*]
        [#:client Client #:token Token]
        [U (HashTable Symbol JSON) Twitter-Error]))
(define user/profile-banner
  (λ (user
      #:client [client (current-client)]
      #:token [token (current-token)])
    (define response
      (oauth/get (twitter/url "/1.1/users/profile_banner.json"
                              `(,(user*->param user)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(hash? x) (json-hash x)]
           [else (response-error 'user/profile-banner x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: user/report-spam
   (->* [Twitter-User*]
        [#:client Client #:token Token]
        [U Twitter-User Twitter-Error]))
(define user/report-spam
  (λ (user
      #:client [client (current-client)]
      #:token [token (current-token)])
    (define response
      (oauth/post (twitter/url "/1.1/users/report_spam.json")
                  client
                  token
                  #:data `(,(user*->param user))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(hash? x) (Twitter-User (json-hash x))]
           [else (response-error 'user/report-spam x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: block/create
   (->* [Twitter-User*]
        [#:client Client #:token Token #:include-entities? (Perhaps Boolean)
                  #:skip-status? (Perhaps Boolean)]
        [U Twitter-User Twitter-Error]))
(define block/create
  (λ (user
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:include-entities? [include-entities? (current-include-entities?)]
      #:skip-status? [skip-status? (current-skip-status?)])
    (define response
      (oauth/post (twitter/url "/1.1/blocks/create.json")
                  client
                  token
                  #:data `(,(user*->param user)
                           ,@(perhaps-param "include_entities" include-entities? bool->string)
                           ,@(perhaps-param "skip_status" skip-status? bool->string))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(hash? x) (Twitter-User (json-hash x))]
           [else (response-error 'block/create x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: block/destroy
   (->* [Twitter-User*]
        [#:client Client #:token Token #:include-entities? (Perhaps Boolean)
                  #:skip-status? (Perhaps Boolean)]
        [U Twitter-User Twitter-Error]))
(define block/destroy
  (λ (user
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:include-entities? [include-entities? (current-include-entities?)]
      #:skip-status? [skip-status? (current-skip-status?)])
    (define response
      (oauth/post (twitter/url "/1.1/blocks/destroy.json")
                  client
                  token
                  #:data `(,(user*->param user)
                           ,@(perhaps-param "include_entities" include-entities? bool->string)
                           ,@(perhaps-param "skip_status" skip-status? bool->string))))
   (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(hash? x) (Twitter-User (json-hash x))]
           [else (response-error 'block/destroy x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: blocks/list
   (->* []
        [#:client Client #:token Token #:cursor (Perhaps Integer)
                  #:include-entities? (Perhaps Boolean) #:skip-status? (Perhaps Boolean)]
        [U (Cursored Twitter-User) Twitter-Error]))
(define blocks/list
  (λ (#:client [client (current-client)] #:token [token (current-token)]
               #:cursor [cursor (Nothing)]
               #:include-entities? [include-entities? (current-include-entities?)]
               #:skip-status? [skip-status? (current-skip-status?)])
    (define ~cursor (enforce-range cursor -1 '+inf))
    (define response
      (oauth/get (twitter/url "/1.1/blocks/list.json"
                              `(,@(perhaps-param "cursor" ~cursor number->string)
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
                          [else (response-error 'blocks/list x)]))]
           [else (response-error 'blocks/list x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: blocks/list-ids
   (->* []
        [#:client Client #:token Token #:cursor (Perhaps Integer)]
        [U (Cursored Twitter-UserID) Twitter-Error]))
(define blocks/list-ids
  (λ (#:client [client (current-client)] #:token [token (current-token)] #:cursor [cursor (Nothing)])
    (define ~cursor (enforce-range cursor -1 '+inf))
    (define response
      (oauth/get (twitter/url "/1.1/blocks/ids.json"
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
                          [else (response-error 'blocks/list-ids x)]))]
           [else (response-error 'blocks/list-ids x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: mute/create
   (->* [Twitter-User*]
        [#:client Client #:token Token]
        [U Twitter-User Twitter-Error]))
(define mute/create
  (λ (user
      #:client [client (current-client)]
      #:token [token (current-token)])
    (define response
      (oauth/post (twitter/url "/1.1/mutes/users/create.json")
                  client
                  token
                  #:data `(,(user*->param user))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(hash? x) (Twitter-User (json-hash x))]
           [else (response-error 'mute/create x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: mute/destroy
   (->* [Twitter-User*]
        [#:client Client #:token Token]
        [U Twitter-User Twitter-Error]))
(define mute/destroy
  (λ (user
      #:client [client (current-client)]
      #:token [token (current-token)])
    (define response
      (oauth/post (twitter/url "/1.1/mutes/users/destroy.json")
                  client
                  token
                  #:data `(,(user*->param user))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(hash? x) (Twitter-User (json-hash x))]
           [else (response-error 'mute/destroy x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: mutes/list
   (->* []
        [#:client Client #:token Token #:cursor (Perhaps Integer)
                  #:include-entities? (Perhaps Boolean) #:skip-status? (Perhaps Boolean)]
        [U (Cursored Twitter-User) Twitter-Error]))
(define mutes/list
  (λ (#:client [client (current-client)] #:token [token (current-token)]
               #:cursor [cursor (Nothing)]
               #:include-entities? [include-entities? (current-include-entities?)]
               #:skip-status? [skip-status? (current-skip-status?)])
    (define ~cursor (enforce-range cursor -1 '+inf))
    (define response
      (oauth/get (twitter/url "/1.1/mutes/users/list.json"
                              `(,@(perhaps-param "cursor" ~cursor number->string)
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
                          [else (response-error 'mutes/list x)]))]
           [else (response-error 'mutess/list x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: mutes/list-ids
   (->* []
        [#:client Client #:token Token #:cursor (Perhaps Integer)]
        [U (Cursored Twitter-UserID) Twitter-Error]))
(define mutes/list-ids
  (λ (#:client [client (current-client)] #:token [token (current-token)] #:cursor [cursor (Nothing)])
    (define ~cursor (enforce-range cursor -1 '+inf))
    (define response
      (oauth/get (twitter/url "/1.1/mutes/users/ids.json"
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
                          [else (response-error 'mutes/list-ids x)]))]
           [else (response-error 'mutes/list-ids x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))
