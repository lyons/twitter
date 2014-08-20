#lang typed/racket/base

(provide (except-out (all-defined-out)
                     member?
                     bool-val
                     bool-or-null?))

(require "../types/types.rkt"
         "../types/json.rkt")
(require "private/data-helpers.rkt")

;; ---------------------------------------------------------------------------------------------------
;; Relationships

(: json->users-relationship (-> (HashTable Symbol JSON) Twitter-Users-Relationship))
(define (json->users-relationship json)
  (define x (hash-ref json 'relationship JSON-Null))
  (assert x hash?)
  (let ([target (hash-ref x 'target JSON-Null)]
        [source (hash-ref x 'source JSON-Null)])
    (cond
      [(and (hash? target) (hash? source))
       (Twitter-Users-Relationship (json-hash target) (json-hash source))]
      [else (error "json->relationship expected relationship object")])))

(: relationship/target-id (-> Twitter-Users-Relationship Twitter-UserID))
(define (relationship/target-id rel)
  (let ([x (hash-ref (Twitter-Users-Relationship-target rel) 'id JSON-Null)])
    (cond
      [(exact-integer? x) (Twitter-UserID x)]
      [else (unpack-error 'relationship/target-id Twitter-UserID x)])))

(: relationship/target-name (-> Twitter-Users-Relationship Twitter-Username))
(define (relationship/target-name rel)
  (let ([x (hash-ref (Twitter-Users-Relationship-target rel) 'screen_name JSON-Null)])
    (cond
      [(string? x) (Twitter-Username x)]
      [else (unpack-error 'relationship/target-name Twitter-Username x)])))

(: relationship/target-following-source? (-> Twitter-Users-Relationship Boolean))
(define (relationship/target-following-source? rel)
  (let ([x (hash-ref (Twitter-Users-Relationship-target rel) 'following JSON-Null)])
    (cond
      [(bool-or-null? x) (bool-val x)]
      [else (unpack-error 'relationship/target-following-source? Boolean x)])))

(: relationship/target-followed-by-source? (-> Twitter-Users-Relationship Boolean))
(define (relationship/target-followed-by-source? rel)
  (let ([x (hash-ref (Twitter-Users-Relationship-target rel) 'followed_by JSON-Null)])
    (cond
      [(bool-or-null? x) (bool-val x)]
      [else (unpack-error 'relationship/target-followed-by-source? Boolean x)])))

(: relationship/source-id (-> Twitter-Users-Relationship Twitter-UserID))
(define (relationship/source-id rel)
  (let ([x (hash-ref (Twitter-Users-Relationship-source rel) 'id JSON-Null)])
    (cond
      [(exact-integer? x) (Twitter-UserID x)]
      [else (unpack-error 'relationship/source-id Twitter-UserID x)])))

(: relationship/source-name (-> Twitter-Users-Relationship Twitter-Username))
(define (relationship/source-name rel)
  (let ([x (hash-ref (Twitter-Users-Relationship-source rel) 'screen_name JSON-Null)])
    (cond
      [(string? x) (Twitter-Username x)]
      [else (unpack-error 'relationship/source-name Twitter-Username x)])))

(: relationship/following? (-> Twitter-Users-Relationship Boolean))
(define (relationship/following? rel)
  (let ([x (hash-ref (Twitter-Users-Relationship-source rel) 'following JSON-Null)])
    (cond
      [(bool-or-null? x) (bool-val x)]
      [else (unpack-error 'relationship/following? Boolean x)])))

(: relationship/followed-by? (-> Twitter-Users-Relationship Boolean))
(define (relationship/followed-by? rel)
  (let ([x (hash-ref (Twitter-Users-Relationship-source rel) 'followed_by JSON-Null)])
    (cond
      [(bool-or-null? x) (bool-val x)]
      [else (unpack-error 'relationship/followed-by? Boolean x)])))

(: relationship/can-dm? (-> Twitter-Users-Relationship Boolean))
(define (relationship/can-dm? rel)
  (let ([x (hash-ref (Twitter-Users-Relationship-source rel) 'can_dm JSON-Null)])
    (cond
      [(bool-or-null? x) (bool-val x)]
      [else (unpack-error 'relationship/can-dm? Boolean x)])))

(: relationship/blocking? (-> Twitter-Users-Relationship Boolean))
(define (relationship/blocking? rel)
  (let ([x (hash-ref (Twitter-Users-Relationship-source rel) 'blocking JSON-Null)])
    (cond
      [(bool-or-null? x) (bool-val x)]
      [else (unpack-error 'relationship/blocking? Boolean x)])))

(: relationship/muting? (-> Twitter-Users-Relationship Boolean))
(define (relationship/muting? rel)
  (let ([x (hash-ref (Twitter-Users-Relationship-source rel) 'muting JSON-Null)])
    (cond
      [(bool-or-null? x) (bool-val x)]
      [else (unpack-error 'relationship/muting? Boolean x)])))

(: relationship/marked-spam? (-> Twitter-Users-Relationship Boolean))
(define (relationship/marked-spam? rel)
  (let ([x (hash-ref (Twitter-Users-Relationship-source rel) 'marked_spam JSON-Null)])
    (cond
      [(bool-or-null? x) (bool-val x)]
      [else (unpack-error 'relationship/marked-spam? Boolean x)])))

(: relationship/all-replies? (-> Twitter-Users-Relationship Boolean))
(define (relationship/all-replies? rel)
  (let ([x (hash-ref (Twitter-Users-Relationship-source rel) 'all_replies JSON-Null)])
    (cond
      [(bool-or-null? x) (bool-val x)]
      [else (unpack-error 'relationship/all-replies? Boolean x)])))

(: relationship/retweets? (-> Twitter-Users-Relationship Boolean))
(define (relationship/retweets? rel)
  (let ([x (hash-ref (Twitter-Users-Relationship-source rel) 'want_retweets JSON-Null)])
    (cond
      [(bool-or-null? x) (bool-val x)]
      [else (unpack-error 'relationship/retweets? Boolean x)])))

(: relationship/notifications? (-> Twitter-Users-Relationship Boolean))
(define (relationship/notifications? rel)
  (let ([x (hash-ref (Twitter-Users-Relationship-source rel) 'notifications_enabled JSON-Null)])
    (cond
      [(bool-or-null? x) (bool-val x)]
      [else (unpack-error 'relationship/notifications? Boolean x)])))

;; ---------------------------------------------------------------------------------------------------
;; Connexions

(: connexions/id (-> Twitter-User-Connexions Twitter-UserID))
(define (connexions/id conn)
  (let ([x (hash-ref (Twitter-User-Connexions-data conn) 'id JSON-Null)])
    (cond
      [(exact-integer? x) (Twitter-UserID x)]
      [else (unpack-error 'connexions/id Twitter-UserID x)])))

(: connexions/name (-> Twitter-User-Connexions Twitter-Username))
(define (connexions/name conn)
  (let ([x (hash-ref (Twitter-User-Connexions-data conn) 'screen_name JSON-Null)])
    (cond
      [(string? x) (Twitter-Username x)]
      [else (unpack-error 'connexions/name Twitter-Username x)])))

(: connexions/display-name (-> Twitter-User-Connexions String))
(define (connexions/display-name conn)
  (let ([x (hash-ref (Twitter-User-Connexions-data conn) 'name JSON-Null)])
    (cond
      [(string? x) x]
      [else (unpack-error 'connexions/display-name String x)])))

(: connexions/following? (-> Twitter-User-Connexions Boolean))
(define (connexions/following? conn)
  (let ([x (hash-ref (Twitter-User-Connexions-data conn) 'connections JSON-Null)])
    (cond
      [(list-of-string? x) (member? "following" x)]
      [else (unpack-error 'connexions/following? Boolean x)])))

(: connexions/following-requested? (-> Twitter-User-Connexions Boolean))
(define (connexions/following-requested? conn)
  (let ([x (hash-ref (Twitter-User-Connexions-data conn) 'connections JSON-Null)])
    (cond
      [(list-of-string? x) (member? "following_requested" x)]
      [else (unpack-error 'connexions/following-requested? Boolean x)])))

(: connexions/followed-by? (-> Twitter-User-Connexions Boolean))
(define (connexions/followed-by? conn)
  (let ([x (hash-ref (Twitter-User-Connexions-data conn) 'connections JSON-Null)])
    (cond
      [(list-of-string? x) (member? "followed_by" x)]
      [else (unpack-error 'connexions/followed-by? Boolean x)])))

(: connexions/blocking? (-> Twitter-User-Connexions Boolean))
(define (connexions/blocking? conn)
  (let ([x (hash-ref (Twitter-User-Connexions-data conn) 'connections JSON-Null)])
    (cond
      [(list-of-string? x) (member? "blocking" x)]
      [else (unpack-error 'connexions/blocking? Boolean x)])))

(: connexions/muting? (-> Twitter-User-Connexions Boolean))
(define (connexions/muting? conn)
  (let ([x (hash-ref (Twitter-User-Connexions-data conn) 'connections JSON-Null)])
    (cond
      [(list-of-string? x) (member? "muting" x)]
      [else (unpack-error 'connexions/muting? Boolean x)])))

(: connexions/none? (-> Twitter-User-Connexions Boolean))
(define (connexions/none? conn)
  (let ([x (hash-ref (Twitter-User-Connexions-data conn) 'connections JSON-Null)])
    (cond
      [(list-of-string? x) (member? "none" x)]
      [else (unpack-error 'connexions/none? Boolean x)])))

;; ---------------------------------------------------------------------------------------------------
;; Helper functions

(: member? (-> String (Listof String) Boolean))
(define (member? str ls)
  (if (member str ls) ; This looks really silly, but member does not return a boolean value
      #t
      #f))

(: bool-val (-> (U 'null Boolean) Boolean))
(define (bool-val b)
  (if (eq? 'null b)
      #f
      b))

(define-predicate bool-or-null? (U Boolean 'null))
