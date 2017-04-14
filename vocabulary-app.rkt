#lang racket

(require web-server/templates
         "vocabulary.rkt"
         "response.rkt"
         "render-base.rkt"
         "tags.rkt")

(provide (all-defined-out))

(define VOCABULARY-TABLE-HEADINGS
  (list "German" "IPA" "Pinyin" "Chinese" "Description"))

;; ========
;; HANDLERS
;; ========
(define (vocabulary-overview-app request)
  (send-success-response
    (render-vocabulary-overview-page)))

(define (vocabulary-app request a-tag)
  (send-success-response
    (render-vocabulary-page a-tag)))

;; =========
;; RENDERING
;; =========
(define (render-vocabulary-overview-page)
  (let ([tags (get-all-vocabulary-tags)])
    (render-base-page
      #:content (include-template "htdocs/templates/vocabulary-overview.html")
      #:page-title "Vocabulary Overview")))

(define (render-vocabulary-page a-tag)
  (let
    ([vocabulary (get-vocabulary-by-tag a-tag)])
    (render-base-page
      #:content (render-vocabulary-table vocabulary)
      #:page-title "Vocabulary"
      #:special-css-imports (render-css-include "/css/vocabulary-table.css"))))

(define (render-vocabulary-table vocabulary)
  (cond [(empty? vocabulary) ""]
        [else
         (let ([headings VOCABULARY-TABLE-HEADINGS]
               [vocabulary vocabulary])
           (string-append
            (render-heading "Vocabulary" #:level 2)
            (include-template "htdocs/templates/vocabulary-table.html")))]))
