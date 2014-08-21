#lang typed/racket/base

(provide timeline/mentions
         timeline/user
         timeline/home
         timeline/retweets)

(require racket/match)
(require oauth/client)
(require "../types/types.rkt"
         "../types/type-helpers.rkt"
         "../types/json.rkt")
(require "../helpers.rkt")
(require "private/api-helpers.rkt")

;; ---------------------------------------------------------------------------------------------------
;; Timelines

(: timeline/mentions
   (->* []
        [#:client Client #:token Token #:count (Perhaps Integer) #:since-id (Perhaps TweetID)
                  #:max-id (Perhaps TweetID)  #:trim-user? (Perhaps Boolean)
                  #:contributor-details? (Perhaps Boolean) #:include-entities? (Perhaps Boolean)]
        [U (Listof Tweet) Twitter-Error]))
(define timeline/mentions
  (λ (#:client [client (current-client)] #:token [token (current-token)]
               #:count [count (Nothing)]
               #:since-id [since-id (Nothing)]
               #:max-id [max-id (Nothing)]
               #:trim-user? [trim-user? (current-trim-user?)]
               #:contributor-details? [details? (Nothing)]
               #:include-entities? [include-entities? (current-include-entities?)])
    (define ~count (enforce-range count 1 200))
    (define response
      (oauth/get (twitter/url "/1.1/statuses/mentions_timeline.json"
                              `(,@(perhaps-param "count" ~count number->string)
                                ,@(perhaps-param "since_id" since-id tweetid->string)
                                ,@(perhaps-param "max_id" max-id tweetid->string)
                                ,@(perhaps-param "trim_user" trim-user? bool->string)
                                ,@(perhaps-param "contributor_details" details? bool->string)
                                ,@(perhaps-param "include_entities" include-entities? bool->string)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(list-of-hash? x) (map (compose Tweet json-hash) x)]
           [else (response-error 'timeline/mentions x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: timeline/user
   (->* [Twitter-User*]
        [#:client Client #:token Token #:count (Perhaps Integer) #:since-id (Perhaps TweetID)
                  #:max-id (Perhaps TweetID) #:exclude-replies? (Perhaps Boolean)
                  #:contributor-details? (Perhaps Boolean) #:include-retweets? (Perhaps Boolean)
                  #:trim-user? (Perhaps Boolean)]
        [U (Listof Tweet) Twitter-Error]))
(define timeline/user
  (λ (user
      #:client [client (current-client)]
      #:token [token (current-token)]
      #:count [count (Nothing)]
      #:since-id [since-id (Nothing)]
      #:max-id [max-id (Nothing)]
      #:exclude-replies? [exclude-replies? (Nothing)]
      #:contributor-details? [details? (Nothing)]
      #:include-retweets? [include-retweets? (Nothing)]
      #:trim-user? [trim-user? (current-trim-user?)])
    (define ~count (enforce-range count 1 200))
    (define response
      (oauth/get (twitter/url "/1.1/statuses/user_timeline.json"
                              `(,(user*->param user)
                                ,@(perhaps-param "count" ~count number->string)
                                ,@(perhaps-param "since_id" since-id tweetid->string)
                                ,@(perhaps-param "max_id" max-id tweetid->string)
                                ,@(perhaps-param "exclude_replies" exclude-replies? bool->string)
                                ,@(perhaps-param "contributor_details" details? bool->string)
                                ,@(perhaps-param "include_rts" include-retweets? bool->string)
                                ,@(perhaps-param "trim_user" trim-user? bool->string)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(list-of-hash? x) (map (compose Tweet json-hash) x)]
           [else (response-error 'timeline/user x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: timeline/home
   (->* []
        [#:client Client #:token Token #:count (Perhaps Integer) #:since-id (Perhaps TweetID)
                  #:max-id (Perhaps TweetID)  #:trim-user? (Perhaps Boolean)
                  #:exclude-replies? (Perhaps Boolean) #:contributor-details? (Perhaps Boolean)
                  #:include-entities? (Perhaps Boolean)]
        [U (Listof Tweet) Twitter-Error]))
(define timeline/home
  (λ (#:client [client (current-client)] #:token [token (current-token)]
               #:count [count (Nothing)]
               #:since-id [since-id (Nothing)]
               #:max-id [max-id (Nothing)]
               #:trim-user? [trim-user? (current-trim-user?)]
               #:exclude-replies? [exclude-replies? (Nothing)]
               #:contributor-details? [details? (Nothing)]
               #:include-entities? [include-entities? (current-include-entities?)])
    (define ~count (enforce-range count 1 200))
    (define response
      (oauth/get (twitter/url "/1.1/statuses/home_timeline.json"
                              `(,@(perhaps-param "count" ~count number->string)
                                ,@(perhaps-param "since_id" since-id tweetid->string)
                                ,@(perhaps-param "max_id" max-id tweetid->string)
                                ,@(perhaps-param "trim_user" trim-user? bool->string)
                                ,@(perhaps-param "exclude_replies" exclude-replies? bool->string)
                                ,@(perhaps-param "contributor_details" details? bool->string)
                                ,@(perhaps-param "include_entities" include-entities? bool->string)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(list-of-hash? x) (map (compose Tweet json-hash) x)]
           [else (response-error 'timeline/home x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

(: timeline/retweets
   (->* []
        [#:client Client #:token Token #:count (Perhaps Integer) #:since-id (Perhaps TweetID)
                  #:max-id (Perhaps TweetID)  #:trim-user? (Perhaps Boolean)
                  #:include-entities? (Perhaps Boolean) #:include-user-entities? (Perhaps Boolean)]
        [U (Listof Tweet) Twitter-Error]))
(define timeline/retweets
  (λ (#:client [client (current-client)] #:token [token (current-token)]
               #:count [count (Nothing)]
               #:since-id [since-id (Nothing)]
               #:max-id [max-id (Nothing)]
               #:trim-user? [trim-user? (current-trim-user?)]
               #:include-entities? [include-entities? (current-include-entities?)]
               #:include-user-entities? [user-entities? (current-include-entities?)])
    (define ~count (enforce-range count 1 200))
    (define response
      (oauth/get (twitter/url "/1.1/statuses/retweets_of_me.json"
                              `(,@(perhaps-param "count" count number->string)
                                ,@(perhaps-param "since_id" since-id tweetid->string)
                                ,@(perhaps-param "max_id" max-id tweetid->string)
                                ,@(perhaps-param "trim_user" trim-user? bool->string)
                                ,@(perhaps-param "include_entities" include-entities? bool->string)
                                ,@(perhaps-param "include_user_entities"
                                                 user-entities?
                                                 bool->string)))
                 client
                 token))
    (match response
      [(HTTP-Response #"HTTP/1.1 200 OK" _ body)
       (let [(x (read-json body))]
         (cond
           [(list-of-hash? x) (map (compose Tweet json-hash) x)]
           [else (response-error 'timeline/retweets x)]))]
      [(HTTP-Response status _ body) (twitter/error status body)])))

