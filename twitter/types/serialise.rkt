#lang typed/racket/base

(provide (all-defined-out))

(require racket/match)
(require "types.rkt"
         "json.rkt")

(: write-twitter-object (-> (U Tweet
                               Twitter-Direct-Message
                               Twitter-Hashtag-Entity
                               Twitter-List
                               Twitter-Media-Entity
                               Twitter-Mention-Entity
                               Twitter-Place
                               Twitter-Symbol-Entity
                               Twitter-Trend
                               Twitter-User
                               Twitter-Users-Relationship
                               Twitter-User-Connexions)
                            Output-Port
                            Void))
(define (write-twitter-object obj port)
  (define data
    (cond
      [(Tweet? obj) (Tweet-data obj)]
      [(Twitter-Direct-Message? obj) (Twitter-Direct-Message-data obj)]
      [(Twitter-Hashtag-Entity? obj) (Twitter-Hashtag-Entity-data obj)]
      [(Twitter-List? obj) (Twitter-List-data obj)]
      [(Twitter-Media-Entity? obj) (Twitter-Media-Entity-data obj)]
      [(Twitter-Mention-Entity? obj) (Twitter-Mention-Entity-data obj)]
      [(Twitter-Place? obj) (Twitter-Place-data obj)]
      [(Twitter-Symbol-Entity? obj) (Twitter-Symbol-Entity-data obj)]
      [(Twitter-Trend? obj) (Twitter-Trend-data obj)]
      [(Twitter-User? obj) (Twitter-User-data obj)]
      [(Twitter-Users-Relationship? obj) `(,(Twitter-Users-Relationship-target obj)
                                           ,(Twitter-Users-Relationship-source obj))]
      [(Twitter-User-Connexions? obj) (Twitter-User-Connexions-data obj)]))
  (write-json data port))

(: twitter-object->json-string (-> (U Tweet
                                      Twitter-Direct-Message
                                      Twitter-Hashtag-Entity
                                      Twitter-List
                                      Twitter-Media-Entity
                                      Twitter-Mention-Entity
                                      Twitter-Place
                                      Twitter-Symbol-Entity
                                      Twitter-Trend
                                      Twitter-User
                                      Twitter-Users-Relationship
                                      Twitter-User-Connexions)
                                   String))
(define (twitter-object->json-string obj)
  (define data
    (cond
      [(Tweet? obj) (Tweet-data obj)]
      [(Twitter-Direct-Message? obj) (Twitter-Direct-Message-data obj)]
      [(Twitter-Hashtag-Entity? obj) (Twitter-Hashtag-Entity-data obj)]
      [(Twitter-List? obj) (Twitter-List-data obj)]
      [(Twitter-Media-Entity? obj) (Twitter-Media-Entity-data obj)]
      [(Twitter-Mention-Entity? obj) (Twitter-Mention-Entity-data obj)]
      [(Twitter-Place? obj) (Twitter-Place-data obj)]
      [(Twitter-Symbol-Entity? obj) (Twitter-Symbol-Entity-data obj)]
      [(Twitter-Trend? obj) (Twitter-Trend-data obj)]
      [(Twitter-User? obj) (Twitter-User-data obj)]
      [(Twitter-Users-Relationship? obj) `(,(Twitter-Users-Relationship-target obj)
                                           ,(Twitter-Users-Relationship-source obj))]
      [(Twitter-User-Connexions? obj) (Twitter-User-Connexions-data obj)]))
  (json->string data))
