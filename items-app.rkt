#lang racket

(require racket/set
         web-server/templates
         "template-procedures.rkt"
         "list-and-set-operations.rkt"
         "render-base.rkt"
         "response.rkt"
         "homework.rkt"
         "homework-app.rkt"
         "vocabulary.rkt"
         "vocabulary-app.rkt")
(provide (all-defined-out))

;; ======================
;; APPS HANDLING REQUESTS
;; ======================

(define (tags-overview-app request)
  (send-success-response
   (render-tags-overview-page)))

(define (items-app request a-tag)
  (cond [(string=? a-tag "")
         (send-success-response
          (render-tags-overview-page))]
        [else
         (send-success-response
          (render-items-page-for-tag a-tag))]))

(define (render-tags-overview-page)
  (let ([tags
         (sort (unique-items-list (append (get-all-homework-tags #:reload true)
                                          (get-all-vocabulary-tags)))
               string<?)])
      (let ([content (include-template "htdocs/templates/tags-overview.html")])
        ;;(display content) (newline)
        (render-base-page #:content content
                          #:page-title "Tags Overview"))))

(define (render-items-page-for-tag a-tag)
  (let ([homeworks-content
         (wrap-rendered #:wrap-name "items-homeworks-container"
                        (render-homeworks (get-homeworks-by-tag a-tag)))]
        [vocabulary-content
         (wrap-rendered #:wrap-name "items-vocabulary-container"
                        (render-vocabulary-table (get-vocabulary-by-tag a-tag)))])
    (let ([content (include-template "htdocs/templates/items.html")]
          [special-css-imports (string-append (render-css-include "/css/homework.css")
                                              (render-css-include "/css/vocabulary-table.css"))])
      ;;(display content) (newline)
      (render-base-page #:content content
                        #:page-title (string-append "Tag: " a-tag)
                        #:special-css-imports special-css-imports))))
