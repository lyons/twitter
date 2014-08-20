#lang typed/racket/base

(provide Twitter-Language-Code)

(define-type Twitter-Language-Code
  (U 'ar    ;; Arabic
     'bn    ;; Bengali
     'cs    ;; Czech
     'da    ;; Danish
     'de    ;; German
     'en    ;; English
     'en-gb ;; EnglishUK
     'es    ;; Spanish
     'fa    ;; Farsi
     'fi    ;; Finnish
     'fil   ;; Filipino
     'fr    ;; French
     'he    ;; Hebrew
     'hi    ;; Hindi
     'hu    ;; Hungarian
     'id    ;; Indonesian
     'it    ;; Italian
     'ja    ;; Japanese
     'ko    ;; Korean
     'msa   ;; Malay
     'nl    ;; Dutch
     'no    ;; Norwegian
     'pl    ;; Polish
     'pt    ;; Portuguese
     'ro    ;; Romanian
     'ru    ;; Russian
     'sv    ;; Swedish
     'th    ;; Thai
     'tr    ;; Turkish
     'uk    ;; Ukrainian
     'vi    ;; Vietnamese
     'zh-cn ;; Simplified Chinese
     'zh-tw ;; Traditional Chinese
     ))

(define-predicate twitter-language-code? Twitter-Language-Code)