#lang racket

(provide/contract
  (start (-> request? response?)))

(require
  web-server/templates
  web-server/servlet-env
  web-server/servlet
  web-server/dispatch)

;; =====
;; STATE
;; =====
(define (get-vocabulary-for-topic topic)
  ;; for now always returns the same
  (list
    (list "sich fuer eine Person entscheiden" "xuǎnzé" "32" "选择")
    (list "teilnehmen" "cānyù" "14" "参与")
    (list "die Wahl" "dàxuǎn" "43" "大选")))

;; ======================
;; APPS HANDLING REQUESTS
;; ======================

(define (vocabulary-app request topic)
  (response/full
    200 #"OK"
    (current-seconds) TEXT/HTML-MIME-TYPE
    empty
    (list (string->bytes/utf-8 (render-vocabulary-page topic)))))

(define (vocabulary-overview-app request)
  (response/xexpr
    `(html
       (head (title "Vocabulary Overview")
             (link ((rel "stylesheet") (href "css/general.css") (type "text/css"))))
       (body (p "This is an overview of vocabulary pages.")))))

(define (overview-app request)
  (response/full
    200 #"OK"
    (current-seconds) TEXT/HTML-MIME-TYPE
    empty
    (list (string->bytes/utf-8 (render-overview-page)))))

;; ===============
;; RENDERING STUFF
;; ===============

(define (render-vocabulary-page topic)
  (let
    ([vocabulary (get-vocabulary-for-topic topic)])
    (let
      ([content (render-vocabulary-table vocabulary)]
        [page-title "Vocabulary"]
        [special-css-imports
          (render-css-include "css/vocabulary-table.css")]
        [special-js-imports ""]
        [header ""]
        [footer ""]
        [navigation ""])
      (include-template
        "templates/base.html"))))

(define (render-vocabulary-table vocabulary)
  (let
    ([table-headers (list "German" "Pinyin" "Tones" "Chinese")]
      [word-list vocabulary])
    (include-template "templates/vocabulary-table.html")))

(define (render-overview-page)
  (let
    ([content
       (let
         ([subpage-titles (list "vocabulary")])
         (include-template "templates/overview.html"))]
      [page-title "Overview"]
      [special-css-imports ""]
      [special-js-imports ""]
      [header ""]
      [footer ""]
      [navigation ""])
    (include-template
      "templates/base.html")))

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
    [("vocabulary" (string-arg)) vocabulary-app]))

(define (respond-unknown-file req)
  (let
    ([content (include-template "templates/unknown-file.html")]
      [page-title "unknown file"]
      [special-css-imports ""]
      [special-js-imports ""]
      [header ""]
      [footer ""]
      [navigation ""])
    (response/full
      404 #"ERROR"
      (current-seconds) TEXT/HTML-MIME-TYPE
      empty
      (list
        (string->bytes/utf-8
          (include-template "templates/base.html"))))))

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
