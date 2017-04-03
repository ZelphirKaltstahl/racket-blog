#lang racket

(provide
  get-homeworks-by-tag
  get-homeworks-by-date
  my-date->string
  make-simple-date-from-iso-string
  make-iso-string-from-date
  HOMEWORK
  homework-text-with-translations
  homework-metadata
  text-with-translations-native-translation
  text-with-translations-native-phonetics
  text-with-translations-foreign-phonetics
  text-with-translations-foreign-translation
  get-all-homework-dates)
(require
  racket/set
  racket/date
  "tags.rkt")


(define nil '())

(struct homework (text-with-translations metadata))
(struct text-with-translations
  (native-translation
    native-phonetics
    foreign-phonetics
    foreign-translation))

;; =======
;; GETTERS
;; =======
(define (get-all-homework-dates)
  (set->list (list->set (map
    (lambda (a-homework)
      (hash-ref (homework-metadata a-homework) 'due-date "no date"))
    HOMEWORK))))

(define (get-homeworks-by-tag a-tag)
  (cond
    [(set-member? TAGS a-tag)
      (filter
        (lambda (a-homework)
          (member
            a-tag
            (hash-ref (homework-metadata (a-homework)) 'tags (list))))
        HOMEWORK)]
    [else nil]))

(define (get-homeworks-by-date a-date)
  (display "getting homeworks for date ") (display a-date) (newline)
  (filter
    (lambda (a-homework)
      (display "date of homework is:       ")
      (display (hash-ref (homework-metadata a-homework) 'due-date ""))
      (newline)
      (equal?
        (hash-ref (homework-metadata a-homework) 'due-date "")
        a-date))
    HOMEWORK))

(define (my-date->string a-date)
  (date-display-format 'iso-8601)
  (let
    ([iso-date (date->string a-date)]
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
      iso-date
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

(define (make-simple-germany-date day month year)
  (date
    0 0 0
    day month year
    (day-month-year->weekday day month year)
    (day-month-year->yearday day month year)
    #t
    7200))

(define (make-iso-string-from-date date)
  (string-join (map number->string (list (date-year date) (date-month date) (date-day date))) "-"))

(define HOMEWORK
  (list
    (homework
      (text-with-translations
        "Chinesisch lernen macht Spaß."
        "IPA"
        "xue2xi2han4yu3hao3wan2"
        "学习汉语好玩。")
      (hash
        'task-description "Schreibe einen Text darueber, ob man nach dem Studium sofort eine Arbeit suchen sollte, oder erst einmal etwas tun sollte, was einem gefaellt."
        'tags (list
                (create-tag! "study")
                (create-tag! "work")
                (create-tag! "essay")
                (create-tag! "article"))
        'due-date (make-simple-germany-date 9 4 2017)))))


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
