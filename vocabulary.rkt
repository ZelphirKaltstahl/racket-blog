#lang racket

(provide (all-defined-out))

(require racket/set
         racket/date
         yaml
         "date-procedures.rkt"
         "list-and-set-operations.rkt")

(define VOCABULARY empty)
;; =======
;; GETTERS
;; =======
(define (reload-vocabulary!)
  (set! VOCABULARY (read-vocabulary-from-directory "content/vocabulary/")))

(define (get-all-vocabulary #:reload [reload true])
  (cond
    [reload (read-vocabulary-from-directory "content/vocabulary/")]
    [else VOCABULARY]))

(define (get-all-vocabulary-tags)
  (unique-items-list
   (flatten
    (map get-complex-word-tags
         (get-all-vocabulary #:reload true)))))

(define (get-all-homework-added-dates)
  (unique-items-list
    (map get-complex-word-added-date
         (get-all-vocabulary #:reload true))))


;; GET SUBSET BY ATTRIBUTE
(define (complex-word-has-tag? a-complex-word a-tag)
  (member a-tag (get-complex-word-tags a-complex-word)))

(define (get-vocabulary-by-tag a-tag)
  (filter (lambda (a-complex-word)
            (complex-word-has-tag? a-complex-word a-tag))
          (get-all-vocabulary #:reload true)))

(define (get-vocabulary-by-date a-date)
  (filter (lambda (a-complex-word)
            (simple-date-equal? (get-complex-word-added-date a-complex-word) a-date))
          (get-all-vocabulary #:reload true)))

;; =================
;; ATTRIBUTE GETTERS
;; =================
(define (get-attribute-from-complex-word a-hash . keys)
  (cond
    [(empty? keys) a-hash]
    [else
     (apply get-attribute-from-complex-word
            (hash-ref a-hash (first keys))
            (rest keys))]))

(define (get-complex-word-added-date complex-word)
  (get-attribute-from-complex-word complex-word "metadata" "added-date"))

(define (get-complex-word-memorization-date complex-word)
  (get-attribute-from-complex-word complex-word "metadata" "memorized-date"))

(define (get-complex-word-author complex-word)
  (get-attribute-from-complex-word complex-word "metadata" "author"))

(define (get-complex-word-description complex-word)
  (get-attribute-from-complex-word complex-word "metadata" "description"))

(define (get-complex-word-tags complex-word)
  (get-attribute-from-complex-word complex-word "metadata" "tags"))

(define (get-complex-word-native-translation complex-word)
  (get-attribute-from-complex-word complex-word "content" "native-translation"))

(define (get-complex-word-foreign-translation complex-word)
  (get-attribute-from-complex-word complex-word "content" "foreign-translation"))

(define (get-complex-word-native-phonetics complex-word)
  (get-attribute-from-complex-word complex-word "content" "native-phonetics"))

(define (get-complex-word-foreign-phonetics complex-word)
  (get-attribute-from-complex-word complex-word "content" "foreign-phonetics"))

;; =================
;; READING THE FILES
;; =================
(define (read-vocabulary-from-file path)
  ;; a vocabulary file can contain more than one word
  (file->yaml* path))

(define (read-vocabulary-from-directory base-path-string)
  (define (concat-with-base-path file-path)
    (build-path base-path-string file-path))

  (define (vocabulary-file? path)
    ;; a vocabulary file must begin with vocabulary
    ;; a vocabulary file must end with yaml
    ;; for example vocabulary-university.yaml
    (let ([file-name (path->string (file-name-from-path path))])
      (and (string-prefix? file-name "vocabulary")
           (string-suffix? file-name "yaml"))))

  (let* ([filesystem-items (directory-list (string->path base-path-string))]
         [files (filter file-exists? (map concat-with-base-path filesystem-items))])
    (flatten (map read-vocabulary-from-file
                  (filter vocabulary-file? files)))))
