#lang typed/racket/base

(provide (except-out (all-defined-out)
                     mode?))

(require (only-in typed/net/url
                  [url URL]
                  string->url))
(require "../types/types.rkt"
         "../types/json.rkt")
(require "private/data-helpers.rkt"
         (only-in "users.rkt"
                  user/id))

;; ---------------------------------------------------------------------------------------------------
;; Lists

(: list/created-at (-> Twitter-List String))
(define (list/created-at ls)
  (let ([x (hash-ref (Twitter-List-data ls) 'created_at JSON-Null)])
    (cond
      [(string? x) x]
      [else (unpack-error 'list/created-at String x)])))

(: list/description (-> Twitter-List String))

(define (list/description ls)
  (let ([x (hash-ref (Twitter-List-data ls) 'description JSON-Null)])
    (cond
      [(string? x) x]
      [else (unpack-error 'list/description String x)])))

(: list/following (-> Twitter-List Boolean))
(define (list/following ls)
  (let ([x (hash-ref (Twitter-List-data ls) 'following JSON-Null)])
    (cond
      [(boolean? x) x]
      [else (unpack-error 'list/following Boolean x)])))

(: list/full-name (-> Twitter-List String))
(define (list/full-name ls)
  (let ([x (hash-ref (Twitter-List-data ls) 'full_name JSON-Null)])
    (cond
      [(string? x) x]
      [else (unpack-error 'list/full-name String x)])))

(: list/id (-> Twitter-List Twitter-ListID))
(define (list/id ls)
  (let ([x (hash-ref (Twitter-List-data ls) 'id JSON-Null)])
    (cond
      [(exact-integer? x) (Twitter-ListID x)]
      [else (unpack-error 'list/id Twitter-ListID x)])))

(: list/member-count (-> Twitter-List Integer))
(define (list/member-count ls)
  (let ([x (hash-ref (Twitter-List-data ls) 'member_count JSON-Null)])
    (cond
      [(exact-integer? x) x]
      [else (unpack-error 'list/member-count Integer x)])))

(: list/mode (-> Twitter-List (U 'public 'private)))
(define (list/mode ls)
  (let ([x (hash-ref (Twitter-List-data ls) 'mode JSON-Null)])
    (cond
      [(string? x)
       (let ([y (string->symbol x)])
         (cond
           [(mode? y) y]
           [else (unpack-error 'list/mode (U 'public 'private) y)]))]
      [else (unpack-error 'list/mode (U 'public 'private) x)])))

(: list/name (-> Twitter-List String))
(define (list/name ls)
  (let ([x (hash-ref (Twitter-List-data ls) 'name JSON-Null)])
    (cond
      [(string? x) x]
      [else (unpack-error 'list/name String x)])))

(: list/owner (-> Twitter-List Twitter-User))
(define (list/owner ls)
  (let ([x (hash-ref (Twitter-List-data ls) 'user JSON-Null)])
    (cond
      [(hash? x) (Twitter-User (json-hash x))]
      [else (unpack-error 'list/owner Twitter-User x)])))

(: list/owner-id (-> Twitter-List Twitter-UserID))
(define (list/owner-id ls)
  (let ([x (list/owner ls)])
    (user/id x)))

(: list/slug (-> Twitter-List String))
(define (list/slug ls)
  (let ([x (hash-ref (Twitter-List-data ls) 'slug JSON-Null)])
    (cond
      [(string? x) x]
      [else (unpack-error 'list/slug String x)])))

(: list/subscriber-count (-> Twitter-List Integer))
(define (list/subscriber-count ls)
  (let ([x (hash-ref (Twitter-List-data ls) 'subscriber_count JSON-Null)])
    (cond
      [(exact-integer? x) x]
      [else (unpack-error 'list/subscriber-count Integer x)])))

(: list/url (-> Twitter-List URL))
(define (list/url ls)
  (let ([x (hash-ref (Twitter-List-data ls) 'uri JSON-Null)])
    (cond
      [(string? x) (string->url x)]
      [else (unpack-error 'list/url URL x)])))

;; ---------------------------------------------------------------------------------------------------
;; Helper functions

(define-predicate mode? (U 'public 'private))
