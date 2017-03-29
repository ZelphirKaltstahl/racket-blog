#lang racket

(provide/contract
  (start (-> request? response?)))

(require
  web-server/templates
  web-server/servlet-env
  web-server/servlet
  web-server/dispatch)

(define nil '())

;; =====
;; STATE
;; =====
(define (create-translation
          first-language-translations
          first-language-phonetics
          second-language-translations
          second-language-extra
          second-language-phonetics)

  (define (translation-concat translations divider result)
    (let
      ([parts (reverse translations)])
      (cond
        [(empty? parts) result]
        [else
          (translation-concat
            (rest parts)
            divider
            (string-append (first parts) divider result))])))
  (list
    (translation-concat first-language-translations "<br>" "")
    (translation-concat first-language-phonetics "<br>" "")
    (translation-concat second-language-phonetics "<br>" "")
    (translation-concat second-language-extra "<br>" "")
    (translation-concat second-language-translations "<br>" "")))

(define VOCABULARY-TOPICS
  (list "other" "politics" "computer"))

(define VOCABULARY-POLITICS
  (list
    (list "sich fuer eine Person entscheiden" "xuǎnzé" "32" "选择")
    (list "teilnehmen" "cānyù" "14" "参与")
    (list "die Wahl" "dàxuǎn" "43" "大选")))

(define VOCABULARY-OTHER
  (list
    (list "a" "b" "c" "d")
    (list "e" "f" "g" "h")))

(define VOCABULARY-COMPUTER
  (list
    (create-translation (list "die Webseite") (list "IPA") (list "wǎngzhàn") (list "34") (list "网站"))
    (create-translation (list "die URL") (list "IPA") (list "wǎngzhàndìzhǐ") (list "3443") (list "网站地址"))
    (create-translation (list "der Onlinekurs") (list "IPA") (list "wǎngkè") (list "34") (list "网课"))
    (create-translation (list "der Kurs") (list "IPA") (list "kèchéng") (list "42") (list "课程"))
    (create-translation (list "das Computerprogramm") (list "IPA") (list "chéngxù") (list "24") (list "程序"))
    (create-translation (list "Data Science") (list "IPA") (list "shùjùkēxué") (list "4412") (list "数据科学"))
    (create-translation (list "Machine Learning") (list "IPA") (list "???") (list "???") (list "???"))
    (create-translation (list "Artificial Intelligence") (list "IPA") (list "???") (list "???") (list "???"))))

(define VOCABULARY
  (hash
    "politics" VOCABULARY-POLITICS
    "computer" VOCABULARY-COMPUTER
    "other" VOCABULARY-OTHER))

(define (get-vocabulary-for-topic topic)
  (hash-ref VOCABULARY topic))
;; IDEA: Tagged vocabulary: just define words with tags and lets them automatically be collected according to topics

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
             (link ((rel "stylesheet") (href "/css/general.css") (type "text/css"))))
       (body (p "This is an overview of vocabulary pages.")))))  ; TODO: how to get a list of available vocabulary subpages?

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
      ([content (render-vocabulary-table vocabulary topic)]
        [page-title "Vocabulary"]
        [special-css-imports
          (render-css-include "/css/vocabulary-table.css")]
        [special-js-imports ""]
        [header ""]
        [footer ""]
        [navigation ""])
      (include-template
        "templates/base.html"))))

(define (render-vocabulary-table vocabulary topic)
  (let
    ([topic topic]
      [table-headers (list "German" "Pinyin" "Tones" "Chinese")]
      [word-list vocabulary])
    (include-template "templates/vocabulary-table.html")))

(define (render-overview-page)
  (let
    ([content
       (let
         ([subpage-titles (list "vocabulary")])  ; TODO: how to dynamically get a list of subpages?
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
