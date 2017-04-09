#lang racket

(require web-server/templates
         "vocabulary.rkt"
         "response.rkt"
         "render-base.rkt"
         "tags.rkt")

(provide (all-defined-out))

;; ========
;; HANDLERS
;; ========
(define (vocabulary-overview-app request)
  (send-success-response
    (render-base-page
      #:content (render-vocabulary-overview-page)
      #:page-title "Vocabulary Overview")))

(define (vocabulary-app request topic)
  (send-success-response
    (render-vocabulary-page topic)))

;; =========
;; RENDERING
;; =========
(define (render-vocabulary-overview-page)
  (let
    ([tags (set->list TAGS)])
    (include-template "htdocs/templates/vocabulary-overview.html")))

(define (render-vocabulary-page topic)
  (let
    ([vocabulary (get-vocabulary-by-tag topic)])
    (render-base-page
      #:content (render-vocabulary-table vocabulary topic)
      #:page-title "Vocabulary"
      #:special-css-imports (render-css-include "/css/vocabulary-table.css"))))

(define (render-vocabulary-table vocabulary topic)
  (let
    ([topic topic]
      [table-headers (list "German" "Pinyin" "Tones" "Chinese" "Description")]
      [word-list vocabulary])
    (include-template "htdocs/templates/vocabulary-table.html")))
