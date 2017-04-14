#lang racket

(provide (all-defined-out))

(require racket/set
         yaml
         "date-procedures.rkt"
         "list-and-set-operations.rkt")

(define HOMEWORK empty)

;; =======
;; GETTERS
;; =======
(define (reload-homework!)
  (set! HOMEWORK (read-homeworks-from-directory "content/homework/")))

(define (get-all-homeworks #:reload [reload false])
  ;; currently always reloading the vocabulary
  (read-homeworks-from-directory "content/homework/"))

(define (get-all-homework-tags #:reload [reload false])
  (unique-items-list
    (flatten (map get-tags-from-homework
                  (get-all-homeworks #:reload reload)))))

(define (get-all-homework-dates)
  (unique-items-list
    (map get-due-date-from-homework
         (get-all-homeworks #:reload true))))

;; GET SUBSET BY ATTRIBUTE
(define (homework-has-tag? a-homework a-tag)
  (member a-tag (get-tags-from-homework a-homework)))

(define (get-homeworks-by-tag a-tag)
  (filter (lambda (a-homework)
            (homework-has-tag? a-homework a-tag))
          (get-all-homeworks #:reload true)))

(define (get-homeworks-by-date a-date)
  (filter (lambda (a-homework)
            (simple-date-equal? (get-due-date-from-homework a-homework) a-date))
          (get-all-homeworks #:reload true)))

;; =================
;; ATTRIBUTE GETTERS
;; =================
(define (get-attribute-from-homework a-hash . keys)
  (cond
    [(empty? keys) a-hash]
    [else
     (apply get-attribute-from-homework
            (hash-ref a-hash (first keys))
            (rest keys))]))

(define (get-due-date-from-homework homework)
  (hash-ref (hash-ref homework "metadata") "due-date" "no date"))

(define (get-tags-from-homework homework)
  (hash-ref (hash-ref homework "metadata") "tags" empty))

(define (get-native-translation homework)
  (get-attribute-from-homework homework "content" "text" "native-translation"))

(define (get-native-translation-corrected homework)
  (get-attribute-from-homework homework "content" "corrected-text" "native-translation"))

(define (get-foreign-translation homework)
  (get-attribute-from-homework homework "content" "text" "foreign-translation"))

(define (get-foreign-translation-corrected homework)
  (get-attribute-from-homework homework "content" "corrected-text" "foreign-translation"))

(define (get-native-phonetics homework)
  (get-attribute-from-homework homework "content" "text" "native-phonetics"))

(define (get-native-phonetics-corrected homework)
  (get-attribute-from-homework homework "content" "corrected-text" "native-phonetics"))

(define (get-foreign-phonetics homework)
  (get-attribute-from-homework homework "content" "text" "foreign-phonetics"))

(define (get-foreign-phonetics-corrected homework)
  (get-attribute-from-homework homework "content" "corrected-text" "foreign-phonetics"))

;; =================
;; READING THE FILES
;; =================
(define (homework-file? path)
  (let ([file-name (path->string (file-name-from-path path))])
    (and (string-prefix? file-name "homework")
         (string-suffix? file-name "yaml"))))

(define (read-homework-from-file path)
  (file->yaml path))

(define (read-homeworks-from-directory base-path-string)
  (define (concat-with-base-path file-path)
    (build-path base-path-string file-path))

  (let* ([filesystem-items (directory-list (string->path base-path-string))]
         [files (filter file-exists? (map concat-with-base-path filesystem-items))])
    (map read-homework-from-file
         (filter homework-file? files))))

;; =================================
;; FROM THE DOCUMENTATION OF RACKET:
;; =================================

;; (struct date
;;   (second
;;    minute
;;    hour
;;    day
;;    month
;;    year
;;    week-day
;;    year-day
;;    dst?
;;    time-zone-offset)
;;   #:extra-constructor-name make-date
;;   #:transparent)
;; second : (integer-in 0 60)
;; minute : (integer-in 0 59)
;; hour : (integer-in 0 23)
;; day : (integer-in 1 31)
;; month : (integer-in 1 12)
;; year : exact-integer?
;; week-day : (integer-in 0 6)
;; year-day : (integer-in 0 365)
;; dst? : boolean?
;; time-zone-offset : exact-integer?

;; for example values: (require racket/date) (seconds->date (current-seconds))
