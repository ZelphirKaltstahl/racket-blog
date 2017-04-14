#lang racket

(provide (all-defined-out))

(define (make-text-with-breaks . parts)
  (string-join parts "<br>"))

(define (format-text-with-newlines-to-html text)
  (string-replace text "\n" "<br>\n"))
