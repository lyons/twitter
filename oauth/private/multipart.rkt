#lang typed/racket/base

(provide (all-defined-out))

(require "../types.rkt"
         "header-helpers.rkt")

(require (only-in racket/file
                  file->bytes)
         (only-in racket/path
                  file-name-from-path))

;; creates a multipart boundary string unlikely to occur in any files included in the multipart data
(: multipart-boundary (-> Bytes))
(define (multipart-boundary)
  (string->bytes/utf-8 (string-append "multipart_boundary_" (oauth-timestamp) (oauth-nonce))))

;; adds prefix and newlines to multipart boundary for use as media section header
(: multipart-boundary-header (-> Bytes Bytes))
(define (multipart-boundary-header boundary)
  (bytes-append #"--" boundary #"\r\n"))

;; converts a file to a multipart data section
(: file->multipart-data (-> Path Bytes Bytes))
(define (file->multipart-data path boundary)
  (define filename (file-name-from-path path))
  (assert filename path-for-some-system?)
  (apply bytes-append
         `(,(multipart-boundary-header boundary)
           #"Content-Type: application/octet-stream\r\n"
           #"Content-Disposition: form-data; name=\"media[]\"; filename=\""
           ,(path->bytes filename) #"\"\r\n\r\n"
           ,(file->bytes path) #"\r\n")))

;; converts an HTTP-Parameter to a multipart data section
(: param->multipart-data (-> HTTP-Parameter Bytes Bytes))
(define (param->multipart-data param boundary)
  (define key   (string->bytes/utf-8 (HTTP-Parameter-key param)))
  (define value (string->bytes/utf-8 (HTTP-Parameter-value param)))
  (apply bytes-append
         `(,(multipart-boundary-header boundary)
           #"Content-Disposition: form-data; name=\"" ,key #"\"\r\n\r\n"
           ,value #"\r\n")))
