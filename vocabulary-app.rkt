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

(define (vocabulary-app request a-tag)
  (send-success-response
    (render-vocabulary-page a-tag)))

;; =========
;; RENDERING
;; =========
(define (render-vocabulary-overview-page)
  (let
    ([tags (set->list TAGS)])
    (include-template "htdocs/templates/vocabulary-overview.html")))

(define (render-vocabulary-page a-tag)
  (let
    ([vocabulary (get-vocabulary-by-tag a-tag)])
    (render-base-page
      #:content (render-vocabulary-table vocabulary)
      #:page-title "Vocabulary"
      #:special-css-imports (render-css-include "/css/vocabulary-table.css"))))

(define (render-vocabulary-table vocabulary)
  (let ([headings (list "German" "IPA" "Pinyin" "Chinese" "Description")]
        [vocabulary vocabulary])
    (include-template "htdocs/templates/vocabulary-table.html")))
