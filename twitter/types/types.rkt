#lang typed/racket/base

(provide (all-defined-out))

(require racket/match)
(require (only-in typed/net/url
                  [url URL]))
(require oauth/types)
(require "json.rkt")

(require "colour.rkt"
         "language.rkt"
         "time-zone.rkt")
(provide (all-from-out "colour.rkt")
         (all-from-out "language.rkt")
         (all-from-out "time-zone.rkt"))

;; ---------------------------------------------------------------------------------------------------
;; Type definitions

(define-type (Perhaps α) (U α Nothing))

(define-type Entity (U Twitter-Hashtag-Entity Twitter-Media-Entity
                       Twitter-Mention-Entity Twitter-URL-Entity))

(define-type Slug+Twitter-User (List String Twitter-User*))

(define-type Twitter-List* (U Twitter-List Twitter-ListID Slug+Twitter-User))

(define-type Twitter-User* (U Twitter-Username Twitter-UserID Twitter-User))

(define-type WOE* (U WOE-ID WOE-Location))

;; ---------------------------------------------------------------------------------------------------
;; Structs

(struct Coördinates
  ([latitude : Inexact-Real]
   [longitude : Inexact-Real]))

(struct (α) Cursored
  ([data : (Listof α)]
   [prev : Integer]
   [next : Integer]))

(struct Nothing ())

(struct Twitter-Error
  ([response-code : Bytes]
   [errors : (Listof (List Integer String))])
  #:transparent)

;; Direct messages ---------------------
(struct Twitter-Direct-Message
  ([data : (HashTable Symbol JSON)]))

(struct Twitter-Direct-MessageID
  ([id : Integer])
  #:transparent)

;; Entities ----------------------------
(struct Twitter-Hashtag-Entity
  ([data : (HashTable Symbol JSON)]))

(struct Twitter-Media-Entity
  ([data : (HashTable Symbol JSON)]))

(struct Twitter-Mention-Entity
  ([data : (HashTable Symbol JSON)]))

(struct Twitter-Symbol-Entity
  ([data : (HashTable Symbol JSON)]))

(struct Twitter-URL-Entity
  ([data : (HashTable Symbol JSON)]))

;; Places ------------------------------
(struct Twitter-Place
  ([data : (HashTable Symbol JSON)]))

(struct Twitter-PlaceID
  ([id : String])
  #:transparent)

;; Trends ------------------------------
(struct Twitter-Trend
  ([data : (HashTable Symbol JSON)]))

;; Where on Earth ID -------------------
(struct WOE-ID
  ([id : Integer])
  #:transparent)

(struct WOE-Location
  ([data : (HashTable Symbol JSON)]))

;; Tweets ------------------------------
(struct Tweet
  ([data : (HashTable Symbol JSON)]))

(struct TweetID
  ([id : Integer])
  #:transparent)

;; Lists -------------------------------
(struct Twitter-List
  ([data : (HashTable Symbol JSON)]))

(struct Twitter-ListID
  ([id : Integer])
  #:transparent)

;; Users -------------------------------
(struct Twitter-User
  ([data : (HashTable Symbol JSON)]))

(struct Twitter-UserID
  ([userid : Integer])
  #:transparent)

(struct Twitter-Username
  ([name : String])
  #:transparent)

;; User relationships ------------------
(struct Twitter-User-Connexions
  ([data : (HashTable Symbol JSON)]))

(struct Twitter-Users-Relationship
  ([target : (HashTable Symbol JSON)]
   [source : (HashTable Symbol JSON)]))

;; ---------------------------------------------------------------------------------------------------
;; Type predicates

(define-predicate list-of-hash? (Listof (HashTable Any Any)))

(define-predicate list-of-integer? (Listof Integer))

(define-predicate list-of-string? (Listof String))
