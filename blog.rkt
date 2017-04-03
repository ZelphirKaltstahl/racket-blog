#lang racket

;; ==============
;; PREDEFINITIONS
;; ==============
(define (Mb-to-B n) (* n 1024 1024))
(define MAX-BYTES (Mb-to-B 64))
(define nil '())
(custodian-limit-memory (current-custodian) MAX-BYTES)

;; =======================
;; PROVIDING AND REQUIRING
;; =======================
(provide/contract
  (start (-> request? response?)))

(require
  web-server/templates
  web-server/servlet-env
  web-server/servlet
  web-server/dispatch
  racket/date
  "tags.rkt"
  "voc.rkt"
  "homework.rkt")

;; ======================
;; APPS HANDLING REQUESTS
;; ======================
(define (send-success-response rendered-page)
  (response/full
    200 #"OK"
    (current-seconds) TEXT/HTML-MIME-TYPE
    empty
    (list (string->bytes/utf-8 rendered-page))))

(define (homework-app request a-date-string)
  (send-success-response
    (render-homeworks-page a-date-string)))

(define (homework-overview-app request)
  (send-success-response (render-homeworks-overview-page)))

(define (vocabulary-app request topic)
  (send-success-response
    (render-vocabulary-page topic)))

(define (overview-app request)
  (send-success-response
    (render-overview-page)))

(define (vocabulary-overview-app request)
  (send-success-response
    (render-base-page
      #:content (render-vocabulary-overview-page)
      #:page-title "Vocabulary Overview")))

(define (items-app request)
  (response/xexpr
    `(html
       (head
         (title "Vocabulary Overview")
         (link ((rel "stylesheet") (href "/css/general.css") (type "text/css"))))
       (body
         (p "This is an overview of vocabulary tags.")
         ))))  ; TODO: how to get a list of available vocabulary subpages?

;; ===============
;; RENDERING STUFF
;; ===============
;; TODO: Get this one working
;; (define render-template
;;   (make-keyword-procedure
;;     (lambda (keywords keyword-args template)
;;       ())))

(define (render-base-page
          #:content [content ""]
          #:page-title [page-title "NO TITLE"]
          #:special-css-imports [special-css-imports ""]
          #:special-js-imports [special-js-imports ""]
          #:header [header (include-template "templates/header.html")]
          #:footer [footer (include-template "templates/footer.html")]
          #:navigation [navigation ""])
  (include-template "templates/base.html"))

(define (render-homeworks-overview-page)
  (let
    ([dates
       (sort
         (get-all-homework-dates)
         #:key my-date->string
         string<?)])
    (include-template "templates/homework-overview.html")))

(define (render-homeworks-page a-date-string)
  (let*
    ([the-date (make-simple-date-from-iso-string a-date-string)]
      [homeworks (get-homeworks-by-date the-date)])
    (render-base-page
      #:content (render-homeworks homeworks)
      #:page-title (string-append "Homework due on " a-date-string))))

(define (render-homeworks homeworks)
  (string-join (map render-homework homeworks) ""))

(define (render-homework homework)
  (let
    ([homework homework])
    (include-template "templates/homework.html")))

(define (render-vocabulary-overview-page)
  (let
    ([tags (set->list TAGS)])
    (include-template "templates/vocabulary-overview.html")))

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
    (include-template "templates/vocabulary-table.html")))

(define (render-overview-page)
  (render-base-page
    #:content (let
                ([subpage-titles (list "vocabulary")])
                (include-template "templates/overview.html"))
    #:page-title "Overview"))

(define (render-css-include path)
  (let
    ([path path])
    (include-template "templates/css-link.html")))


;; ====================
;; ROUTES MANAGING CODE
;; ====================
(define (start request)
  ;; for now only calling the dispatch
  ;; we could put some action here, which shall happen before dispatch
  (blog-dispatch request))

(define-values (blog-dispatch blug-url)
  (dispatch-rules
    [("index") overview-app]
    [("vocabulary") vocabulary-overview-app]
    [("vocabulary" (string-arg)) vocabulary-app]
    [("homework") homework-overview-app]
    [("homework" (string-arg)) homework-app]
    [("items") items-app]))

(define (respond-unknown-file req)
  (response/full
    404 #"ERROR"
    (current-seconds) TEXT/HTML-MIME-TYPE
    empty
    (list
      (string->bytes/utf-8
        (render-base-page
          #:content (include-template "templates/unknown-route.html")
          #:page-title "unknown route")))))

;; ===========================
;; ADDED FOR RUNNING A SERVLET
;; ===========================
(serve/servlet
  start
  #:servlet-path "/index"  ; default URL
  #:extra-files-paths (list (build-path (current-directory) "static"))  ; directory for static files
  #:port 8000 ; the port on which the servlet is running
  #:servlet-regexp #rx""
  #:launch-browser? true  ; should racket show the servlet running in a browser upon startup?
  ;; #:quit? false  ; ???
  #:listen-ip false  ; the server will listen on ALL available IP addresses, not only on one specified
  #:server-root-path (current-directory)
  #:file-not-found-responder respond-unknown-file)
