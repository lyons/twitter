#lang typed/racket/base

(provide direct-message/create direct-message/show direct-message/destroy
         direct-messages/list direct-messages/list-sent)

(require racket/match)
(require oauth/client)
(require "../types/types.rkt"
         "../types/type-helpers.rkt"
         "../types/json.rkt")
(require "../helpers.rkt")
(require "private/api-helpers.rkt")

;; ---------------------------------------------------------------------------------------------------
;; Twitter-Direct messages

(: direct-message/create
   (->* [Twitter-User* String]
        [#:client Client #:token Token]
        [U Twitter-Direct-Message Twitter-Error]))
(define direct-message/create
  (λ (user
      text
      #:client [client (current-client)]
      #:token [token (current-token)])
    (define response
      (oauth/post (twitter/url "/1.1/direct_messages/new.json")
                  client
                  token
                  #:data `(,(Param "text" text)
                           ,(user*->param user))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(hash? x) (Twitter-Direct-Message (json-hash x))]
           [else (response-error 'direct-message/create x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: direct-message/show
   (->* [Twitter-Direct-MessageID]
        [#:client Client #:token Token]
        [U Twitter-Direct-Message Twitter-Error]))
(define direct-message/show
  (λ (id
      #:client [client (current-client)]
      #:token [token (current-token)])
    (define response
      (oauth/get (twitter/url (string-append "/1.1/direct_messages/show.json?id="
                                             (direct-message-id->string id)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(hash? x) (Twitter-Direct-Message (json-hash x))]
           [else (response-error 'direct-message/show x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: direct-message/destroy
   (->* [Twitter-Direct-MessageID]
        [#:client Client #:token Token #:include-entities? (Perhaps Boolean)]
        [U Twitter-Direct-Message Twitter-Error]))
(define direct-message/destroy
  (λ (id
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:include-entities? [include-entities? (current-include-entities?)])
    (define response
      (oauth/post (twitter/url "/1.1/direct_messages/destroy.json")
                  client
                  token
                  #:data `(,(Param "id" (direct-message-id->string id))
                           ,@(perhaps-param "include_entities" include-entities? bool->string))))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(hash? x) (Twitter-Direct-Message (json-hash x))]
           [else (response-error 'direct-message/destroy x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: direct-messages/list
   (->* []
        [#:client Client #:token Token #:since-id (Perhaps Twitter-Direct-MessageID)
                  #:max-id (Perhaps Twitter-Direct-MessageID) #:count (Perhaps Integer)
                  #:include-entities? (Perhaps Boolean) #:skip-status? (Perhaps Boolean)]
        [U (Listof Twitter-Direct-Message) Twitter-Error]))
(define direct-messages/list
  (λ (#:client [client (current-client)] #:token [token (current-token)]
               #:since-id [since-id (Nothing)]
               #:max-id [max-id (Nothing)]
               #:count [count (Nothing)]
               #:include-entities? [include-entities? (current-include-entities?)]
               #:skip-status? [skip-status? (current-skip-status?)])
    (define ~count (enforce-range count 1 200))
    (define response
      (oauth/get (twitter/url "/1.1/direct_messages.json"
                              `(,@(perhaps-param "since_id" since-id direct-message-id->string)
                                ,@(perhaps-param "max_id" max-id direct-message-id->string)
                                ,@(perhaps-param "count" ~count number->string)
                                ,@(perhaps-param "include_entities" include-entities? bool->string)
                                ,@(perhaps-param "skip_status" skip-status? bool->string)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(list-of-hash? x) (map (compose Twitter-Direct-Message json-hash) x)]
           [else (response-error 'direct-messages/list x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: direct-messages/list-sent
   (->* []
        [#:client Client #:token Token #:since-id (Perhaps Twitter-Direct-MessageID)
                  #:max-id (Perhaps Twitter-Direct-MessageID) #:count (Perhaps Integer)
                  #:page (Perhaps Integer) #:include-entities? (Perhaps Boolean)]
        [U (Listof Twitter-Direct-Message) Twitter-Error]))
(define direct-messages/list-sent
  (λ (#:client [client (current-client)] #:token [token (current-token)]
               #:since-id [since-id (Nothing)]
               #:max-id [max-id (Nothing)]
               #:count [count (Nothing)]
               #:page [page (Nothing)]
               #:include-entities? [include-entities? (current-include-entities?)])
    (define ~count (enforce-range count 1 200))
    (define ~page (enforce-range page 1 '+inf))
    (define response
      (oauth/get (twitter/url "/1.1/direct_messages/sent.json"
                              `(,@(perhaps-param "since_id" since-id direct-message-id->string)
                                ,@(perhaps-param "max_id" max-id direct-message-id->string)
                                ,@(perhaps-param "count" ~count number->string)
                                ,@(perhaps-param "page" ~page number->string)
                                ,@(perhaps-param "include_entities" include-entities? bool->string)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let ([x (read-json body)])
         (cond
           [(list-of-hash? x) (map (compose Twitter-Direct-Message json-hash) x)]
           [else (response-error 'direct-messages/list-sent x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))
