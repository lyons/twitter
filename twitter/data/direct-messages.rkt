#lang typed/racket/base

(provide (all-defined-out))

(require racket/match)
(require "../types/types.rkt"
         "../types/json.rkt")
(require "private/data-helpers.rkt")

;; ---------------------------------------------------------------------------------------------------
;; Direct Messages

(: direct-message/created-at (-> Twitter-Direct-Message String))
(define (direct-message/created-at dm)
  (let ([x (hash-ref (Twitter-Direct-Message-data dm) 'created_at JSON-Null)])
    (cond
      [(string? x) x]
      [else (unpack-error 'direct-message/created-at String x)])))

(: direct-message/hashtag-entities (-> Twitter-Direct-Message (Listof Twitter-Hashtag-Entity)))
(define (direct-message/hashtag-entities dm)
  (let ([x (hash-ref (Twitter-Direct-Message-data dm) 'entities JSON-Null)])
    (cond
      [(hash? x) (let ([y (hash-ref x 'hashtags JSON-Null)])
                   (cond
                     [(list-of-hash? y) (map (compose Twitter-Hashtag-Entity json-hash) y)]
                     [else '()]))]
      [else '()])))

(: direct-message/id (-> Twitter-Direct-Message Twitter-Direct-MessageID))
(define (direct-message/id dm)
  (let ([x (hash-ref (Twitter-Direct-Message-data dm) 'id JSON-Null)])
    (cond
      [(exact-integer? x) (Twitter-Direct-MessageID x)]
      [else (unpack-error 'direct-message/id Twitter-Direct-MessageID x)])))

(: direct-message/media-entities (-> Twitter-Direct-Message (Listof Twitter-Media-Entity)))
(define (direct-message/media-entities dm)
  (let ([x (hash-ref (Twitter-Direct-Message-data dm) 'entities JSON-Null)])
    (cond
      [(hash? x) (let ([y (hash-ref x 'media JSON-Null)])
                   (cond
                     [(list-of-hash? y) (map (compose Twitter-Media-Entity json-hash) y)]
                     [else '()]))]
      [else '()])))

(: direct-message/mention-entities (-> Twitter-Direct-Message (Listof Twitter-Mention-Entity)))
(define (direct-message/mention-entities dm)
  (let ([x (hash-ref (Twitter-Direct-Message-data dm) 'entities JSON-Null)])
    (cond
      [(hash? x) (let ([y (hash-ref x 'user_mentions JSON-Null)])
                   (cond
                     [(list-of-hash? y) (map (compose Twitter-Mention-Entity json-hash) y)]
                     [else '()]))]
      [else '()])))

(: direct-message/recipient (-> Twitter-Direct-Message Twitter-User))
(define (direct-message/recipient dm)
  (let ([x (hash-ref (Twitter-Direct-Message-data dm) 'recipient JSON-Null)])
    (cond
      [(hash? x) (Twitter-User (json-hash x))]
      [else (unpack-error 'direct-message/recipient Twitter-User x)])))

(: direct-message/sender-id (-> Twitter-Direct-Message Twitter-UserID))
(define (direct-message/sender-id dm)
  (let ([x (hash-ref (Twitter-Direct-Message-data dm) 'sender_id JSON-Null)])
    (cond
      [(exact-integer? x) (Twitter-UserID x)]
      [else (unpack-error 'direct-message/sender-id Twitter-UserID x)])))

(: direct-message/sender-name (-> Twitter-Direct-Message Twitter-Username))
(define (direct-message/sender-name dm)
  (let ([x (hash-ref (Twitter-Direct-Message-data dm) 'sender_screen_name JSON-Null)])
    (cond
      [(string? x) (Twitter-Username x)]
      [else (unpack-error 'direct-message/sender-name Twitter-Username x)])))

(: direct-message/symbol-entities (-> Twitter-Direct-Message (Listof Twitter-Symbol-Entity)))
(define (direct-message/symbol-entities dm)
  (let ([x (hash-ref (Twitter-Direct-Message-data dm) 'entities JSON-Null)])
    (cond
      [(hash? x) (let ([y (hash-ref x 'symbols JSON-Null)])
                   (cond
                     [(list-of-hash? y) (map (compose Twitter-Symbol-Entity json-hash) y)]
                     [else '()]))]
      [else '()])))

(: direct-message/text (-> Twitter-Direct-Message String))
(define (direct-message/text dm)
  (let ([x (hash-ref (Twitter-Direct-Message-data dm) 'text JSON-Null)])
    (cond
      [(string? x) x]
      [else (unpack-error 'direct-message/text String x)])))

(: direct-message/url-entities (-> Twitter-Direct-Message (Listof Twitter-URL-Entity)))
(define (direct-message/url-entities dm)
  (let ([x (hash-ref (Twitter-Direct-Message-data dm) 'entities JSON-Null)])
    (cond
      [(hash? x) (let ([y (hash-ref x 'urls JSON-Null)])
                   (cond
                     [(list-of-hash? y) (map (compose Twitter-URL-Entity json-hash) y)]
                     [else '()]))]
      [else '()])))
