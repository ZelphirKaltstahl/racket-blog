#lang racket

;; ==============
;; PREDEFINITIONS
;; ==============
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 128))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)

;; =======================
;; PROVIDING AND REQUIRING
;; =======================
(provide/contract
  (start (-> request? response?)))

(require web-server/templates
         web-server/servlet-env
         web-server/servlet
         web-server/dispatch
         racket/date
         "tags.rkt"
         "vocabulary-app.rkt"
         "overview-app.rkt"
         "homework-app.rkt"
         "items-app.rkt"
         "response.rkt"
         "render-base.rkt"
         "homework.rkt")

;; ====================
;; ROUTES MANAGING CODE
;; ====================
(define (start request)
  ;; for now only calling the dispatch
  ;; we could put some action here, which shall happen before each dispatching
  (blog-dispatch request))

(define-values (blog-dispatch blog-url)
  (dispatch-rules [("")
                   #:method "get"
                   overview-app]

                  [("index")
                   #:method "get"
                   overview-app]

                  [("vocabulary")
                   #:method "get"
                   vocabulary-overview-app]

                  [("vocabulary" (string-arg))
                   #:method "get"
                   vocabulary-app]

                  [("homework")
                   #:method "get"
                   homework-overview-app]

                  [("homework" (string-arg))
                   #:method "get"
                   homework-app]

                  [("tags")
                   #:method "get"
                   tags-overview-app]
                  [("tags" (string-arg))
                   #:method "get"
                   items-app]))

;; This procedure is still here, because it belongs to the main functionality of the whole blog, to display a page, when something could not be found.
(define (respond-unknown-file req)
  (let ([rendered-page (render-base-page #:content (include-template "htdocs/templates/unknown-route.html")
                                         #:page-title "unknown route")])
    (make-response #:code 404
                   #:message #"ERROR"
                   rendered-page)))

;; ===========================
;; ADDED FOR RUNNING A SERVLET
;; ===========================
(serve/servlet
  start
  #:servlet-path "/index"  ; default URL
  #:extra-files-paths (list (build-path (current-directory) "static"))  ; directory for static files
  #:port 8000 ; the port on which the servlet is running
  #:servlet-regexp #rx""
  #:launch-browser? false  ; should racket show the servlet running in a browser upon startup?
  ;; #:quit? false  ; ???
  #:listen-ip false  ; the server will listen on ALL available IP addresses, not only on one specified
  #:server-root-path (current-directory)
  #:file-not-found-responder respond-unknown-file)

;; from the Racket documentation:
;; When you use web-server/dispatch with serve/servlet, you almost always want to use the #:servlet-regexp argument with the value "" to capture all top-level requests. However, make sure you don’t include an else in your rules if you are also serving static files, or else the filesystem server will never see the requests.
;; https://docs.racket-lang.org/web-server/dispatch.html
