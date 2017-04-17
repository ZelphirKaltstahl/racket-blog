#lang racket

(require web-server/templates)
(provide (all-defined-out))

(define (wrap-rendered #:wrap-name [wrap-name ""] content)
  (let ([wrap-name wrap-name]
        [content content])
    (include-template "htdocs/templates/wrap.html")))

(define (make-text-with-breaks . parts)
  (string-join parts "<br>"))

(define (format-text-with-newlines-to-html text)
  (string-replace text "\n" "<br>\n"))
