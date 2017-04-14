#lang racket

(require racket/set)
(provide (all-defined-out))

(define (unique-items-list a-list)
  (set->list (list->set a-list)))
