#lang typed/racket/base

(provide (all-defined-out))

(require (only-in typed/net/url
                  [url URL]
                  string->url))
(require "../types/types.rkt"
         "../types/json.rkt")
(require "private/data-helpers.rkt")

(: trend/events (-> Twitter-Trend Any)) ; What type is this supposed to return?
(define (trend/events trend)
  (let ([x (hash-ref (Twitter-Trend-data trend) 'events JSON-Null)])
    (cond
      [x x]
      [else (unpack-error 'trend/name Any x)])))

(: trend/name (-> Twitter-Trend String))
(define (trend/name trend)
  (let ([x (hash-ref (Twitter-Trend-data trend) 'name JSON-Null)])
    (cond
      [(string? x) x]
      [else (unpack-error 'trend/name String x)])))

(: trend/promoted-content (-> Twitter-Trend Any)) ; What type is this supposed to return?
(define (trend/promoted-content trend)
  (let ([x (hash-ref (Twitter-Trend-data trend) 'promoted_content JSON-Null)])
    (cond
      [x x]
      [else (unpack-error 'trend/promoted-content Any x)])))

(: trend/query (-> Twitter-Trend String))
(define (trend/query trend)
  (let ([x (hash-ref (Twitter-Trend-data trend) 'query JSON-Null)])
    (cond
      [(string? x) x]
      [else (unpack-error 'trend/query String x)])))

(: trend/url (-> Twitter-Trend URL))
(define (trend/url trend)
  (let ([x (hash-ref (Twitter-Trend-data trend) 'url JSON-Null)])
    (cond
      [(string? x) (string->url x)]
      [else (unpack-error 'trend/url URL x)])))