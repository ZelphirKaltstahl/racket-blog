#lang racket

(provide (all-defined-out))
(require
  racket/set)

(define TAGS (mutable-set))

(define (create-tag! a-tag-name)
  (set-add! TAGS a-tag-name)
  a-tag-name)
