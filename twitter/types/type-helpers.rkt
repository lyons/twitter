#lang typed/racket/base

(provide (all-defined-out))

(require racket/match)
(require "../types/types.rkt")
(require (only-in "../data/users.rkt"
                  user/id))
(require (only-in "../data/lists.rkt"
                  list/id))
(require (only-in "../oauth/oauth.rkt"
                  HTTP-Parameter
                  HTTP-Parameter-List
                  Param))

;; ---------------------------------------------------------------------------------------------------
;; Type helpers

;; Base types --------------------------
(: bool->string (-> Boolean String))
(define (bool->string b)
  (if b "true" "false"))

(: string-identity (-> String String))
(define (string-identity str)
  str)

;; Coördinates -------------------------
(: coördinates->perhaps-params (-> (Perhaps Coördinates) HTTP-Parameter-List))
(define (coördinates->perhaps-params coörds)
  (match coörds
    [(Nothing) '()]
    [(Coördinates lat long) `(,(Param "lat" (number->string lat))
                              ,(Param "long" (number->string long)))]))

;; Direct messages ---------------------
(: direct-message-id->string (-> Twitter-Direct-MessageID String))
(define (direct-message-id->string id)
  (number->string (Twitter-Direct-MessageID-id id)))

;; Lists -------------------------------
(: tw-list*->params (-> Twitter-List* HTTP-Parameter-List))
(define (tw-list*->params list)
  (match list
    [(Twitter-ListID id) `(,(Param "list_id" (number->string id)))]
    [(list slug user) `(,(Param "slug" slug)
                        ,(Param (if (Twitter-Username? user) "owner_screen_name" "owner_id")
                                (user*->string user)))]
    [x #:when (Twitter-List? x) (tw-list*->params (list/id x))]))

;; Places ------------------------------
(: placeid->string (-> Twitter-PlaceID String))
(define (placeid->string place)
  (Twitter-PlaceID-id place))

;; Users -------------------------------
(: user*->param (-> Twitter-User* HTTP-Parameter))
(define (user*->param user)
  (match user
    [(Twitter-Username name) (Param "screen_name" name)]
    [(Twitter-UserID userid) (Param "user_id" (number->string userid))]
    [x #:when (Twitter-User? x) (user*->param (user/id x))]))

(: user*->spliceable-param (-> (Perhaps Twitter-User*) (U HTTP-Parameter-List Null)))
(define (user*->spliceable-param user)
  (match user
    [(Twitter-Username name) `(,(Param "screen_name" name))]
    [(Twitter-UserID userid) `(,(Param "user_id" (number->string userid)))]
    [(Nothing) '()]
    [x #:when (Twitter-User? x) (user*->spliceable-param (user/id x))]))

(: user*->string (-> Twitter-User* String))
(define (user*->string user)
  (match user
    [(Twitter-Username name) name]
    [(Twitter-UserID userid) (number->string userid)]
    [x #:when (Twitter-User? x) (user*->string (user/id x))]))

;; Tweets ------------------------------
(: tweetid->string (-> TweetID String))
(define (tweetid->string id)
  (number->string (TweetID-id id)))

;; Where on Earth ID -------------------
(: woe-id->string (-> WOE-ID String))
(define (woe-id->string id)
  (number->string (WOE-ID-id id)))

(: woe*->param (-> WOE* HTTP-Parameter))
(define (woe*->param woe)
  (match woe
    [(WOE-ID id) (Param "id" (number->string id))]
    [x #:when (WOE-Location? x) (woe*->param (woe-location/id x))]))

(: woe-location/id (-> WOE-Location WOE-ID))
(define (woe-location/id loc)
  (WOE-ID 1))
