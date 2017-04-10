#lang racket

(require web-server/templates
         "render-base.rkt"
         "response.rkt")
(provide (all-defined-out))

(define (overview-app request)
  (send-success-response
    (render-overview-page)))

(define (render-overview-page)
  (render-base-page
    #:content (let
                ([subpage-titles (list "tags" "vocabulary" "homework")])
                (include-template "htdocs/templates/overview.html"))
    #:page-title "Overview"))
