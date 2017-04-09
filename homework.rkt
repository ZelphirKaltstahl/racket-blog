#lang racket

(provide (all-defined-out))

(require
  racket/set
  racket/date
  yaml
  "tags.rkt")

(define HOMEWORK empty)

;; =======
;; GETTERS
;; =======
(define (reload-homework!)
  (set! HOMEWORK (read-homeworks-from-directory "content/homework/")))

(define (get-all-homeworks #:reload [reload false])
  (read-homeworks-from-directory "content/homework/"))

(define (get-all-tags #:reload [reload false])
  (unique-items-list
    (map get-tags-from-homework
         (get-all-homeworks #:reload reload))))

(define (get-due-date-from-homework homework)
  (hash-ref (hash-ref homework "metadata") "due-date" "no date"))

(define (get-tags-from-homework homework)
  (hash-ref (hash-ref homework "metadata") "tags" empty))

(define (get-all-homework-dates)
  (unique-items-list
    (map get-due-date-from-homework
         (get-all-homeworks #:reload true))))

(define (homework-has-tag? a-homework a-tag)
  (member a-tag (get-tags-from-homework a-homework)))

(define (get-homeworks-by-tag a-tag)
  (filter (lambda (a-homework)
            (member a-tag (get-tags-from-homework a-homework)))
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

;; ====================
;; AUXILIARY PROCEDURES
;; ====================

(define (unique-items-list a-list)
  (set->list (list->set a-list)))

;; === WORKING WITH DATES ===
(define (simple-date-equal? date1 date2)
  (and (equal? (date-year date1) (date-year date2))
       (equal? (date-month date1) (date-month date2))
       (equal? (date-day date1) (date-day date2))))

(define (my-date->string a-date)
  (date-display-format 'iso-8601)
  (define (at-least-two-digits num-string)
    (if (= (string-length num-string) 1)
        (string-append "0" num-string)
        num-string))
  (let
    ([iso-date-string (date->string a-date)]
      [week-day-number-to-string
        (hash
          0 "Sun"
          1 "Mon"
          2 "Tue"
          3 "Wed"
          4 "Thu"
          5 "Fri"
          6 "Sat")])
    (string-append
     (string-join (map at-least-two-digits (string-split iso-date-string "-")) "-")
     ", "
     (hash-ref week-day-number-to-string (date-week-day a-date) "unknown"))))

(define (make-simple-date-from-iso-string iso-date-string)
  (let*
    ([parts (string-split iso-date-string "-")]
      [year (string->number (first parts))]
      [month (string->number (second parts))]
      [day (string->number (third parts))])
    (make-simple-germany-date day month year)))

;; Given a day, month, and year, return the weekday
;; http://stackoverflow.com/a/13432738/1829329
(define (day-month-year->weekday day month year)
  (let*
    ([local-secs (find-seconds 0 0 0 day month year #t)]
      [the-date (seconds->date local-secs)])
    (date-week-day the-date)))

;; Given a day, month, and year, return the year-day
;; http://stackoverflow.com/a/13432738/1829329
(define (day-month-year->yearday day month year)
  (let*
    ([local-secs (find-seconds 0 0 0 day month year #t)]
      [the-date (seconds->date local-secs)])
    (date-year-day the-date)))

(define (make-iso-string-from-date date)
  (string-join (map number->string
                    (list (date-year date)
                          (date-month date)
                          (date-day date)))
               "-"))

(define (make-simple-germany-date day month year)
  (date
    0 0 0
    day month year
    (day-month-year->weekday day month year)
    (day-month-year->yearday day month year)
    #t
    7200))

;; === WORKING WITH TEXT ===
(define (make-text-with-breaks . parts)
  (string-join parts "<br>"))

(define (format-text-with-newlines-to-html text)
  (string-replace text "\n" "<br>\n"))

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
