#lang racket

(require web-server/servlet-env
         web-server/servlet
         "render-base.rkt"
         "response.rkt")

(provide (all-defined-out))

;; ======================
;; APPS HANDLING REQUESTS
;; ======================
(define (items-app request)
  (response/xexpr
    `(html
       (head
         (title "Vocabulary Overview")
         (link ((rel "stylesheet") (href "/css/general.css") (type "text/css"))))
       (body
         (p "This is an overview of vocabulary tags.")
         ))))  ; TODO: how to get a list of available vocabulary subpages?
