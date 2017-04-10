#lang racket

(require web-server/templates
         "homework.rkt"
         "response.rkt"
         "render-base.rkt")
(provide (all-defined-out))


(define (homework-app request a-date-string)
  (let* ([the-date (make-simple-date-from-iso-string a-date-string)]
         [homeworks (get-homeworks-by-date the-date)])
    (send-success-response
     (render-homeworks-page homeworks))))

(define (homework-overview-app request)
  (send-success-response (render-homeworks-overview-page)))

;; ===============
;; RENDERING STUFF
;; ===============
(define (render-homeworks-overview-page)
  (render-base-page
   #:page-title "Homeworks Overview"
   #:content (let ([dates (sort (get-all-homework-dates)
                                #:key my-date->string
                                string<?)])
               (include-template "htdocs/templates/homework-overview.html"))))

(define (render-homeworks-page homeworks)
  (render-base-page #:content (render-homeworks homeworks)
                    #:special-css-imports (render-css-include "/css/homework.css")
                    #:page-title (string-append "Homework")))

(define (render-homeworks homeworks)
  (string-join (map render-homework homeworks) ""))

(define (render-homework homework)
  (let
    ([homework homework])
    (include-template "htdocs/templates/homework.html")))
