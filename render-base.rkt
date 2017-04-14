#lang racket

(require web-server/templates)

(provide (all-defined-out))


(define (dynamic-include-template path)
  (eval #`(include-template #,path)))

(define render-template
  (make-keyword-procedure
   (lambda (kws kw-args [path "htdocs/templates/base.html"] [content "<p>no content!</p>"])
     (for ([keyword kws]
           [keyword-arg kw-args])
       (namespace-set-variable-value!
        (string->symbol
         (keyword->string keyword))
        keyword-arg))
     (namespace-set-variable-value! 'content content)
     (dynamic-include-template path))))

(define (render-base-page #:content [content ""]
                          #:page-title [page-title "NO TITLE"]
                          #:special-css-imports [special-css-imports ""]
                          #:special-js-imports [special-js-imports ""]
                          #:header [header (include-template "htdocs/templates/header.html")]
                          #:footer [footer (include-template "htdocs/templates/footer.html")]
                          #:navigation [navigation ""])
  (include-template "htdocs/templates/base.html"))

(define (render-heading text #:level [level 1])
  (let ([level level]
        [text text])
    (cond
      [(= level 1) (include-template "htdocs/templates/heading-1.html")]
      [(= level 2) (include-template "htdocs/templates/heading-2.html")]
      [(= level 3) (include-template "htdocs/templates/heading-3.html")]
      [(= level 4) (include-template "htdocs/templates/heading-4.html")]
      [(= level 5) (include-template "htdocs/templates/heading-5.html")]
      [(= level 6) (include-template "htdocs/templates/heading-6.html")])))

(define (render-css-include path)
  (let ([path path])
    (include-template "htdocs/templates/css-link.html")))
