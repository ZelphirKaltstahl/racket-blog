#lang racket

(provide (all-defined-out))

(define (my-hash-map h f)
  (make-immutable-hash
   (hash-map h
             (lambda (k v) (f k v)))))
