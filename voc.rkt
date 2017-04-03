#lang racket

(provide (all-defined-out))
(require
  racket/set
  "tags.rkt")

(define nil '())

(struct complex-word (word metadata))
(struct word
  (first-language-translations
    first-language-phonetics
    second-language-translations
    second-language-extra
    second-language-phonetics))

;; =======
;; GETTERS
;; =======
(define (get-tag a-complex-word)
  (hash-ref (complex-word-metadata a-complex-word) 'tags (list)))

(define (get-description a-complex-word)
  (hash-ref (complex-word-metadata a-complex-word) 'description ""))

(define (get-vocabulary-by-tag a-tag)
  (cond
    [(set-member? TAGS a-tag)
      (filter
        (lambda (word) (member a-tag (get-tag word)))
        VOCABULARY)]
    [else nil]))

;; =====
;; STATE
;; =====
(define VOCABULARY
  (list
    (complex-word
      (word
        '("die Webseite")
        '("IPA")
        '("wǎngzhàn")
        '("34")
        '("网站"))
      (hash
        'description ""
        'tags (list (create-tag! "computer") (create-tag! "IT"))))
    (complex-word
      (word
        '("die URL")
        '("IPA")
        '("wǎngzhàndìzhǐ")
        '("3443")
        '("网站地址"))
      (hash
        'description ""
        'tags (list (create-tag! "computer"))))
    (complex-word
      (word
        '("der Onlinekurs")
        '("IPA")
        '("wǎngkè")
        '("34")
        '("网课"))
      (hash
        'description ""
        'tags (list (create-tag! "computer"))))
    (complex-word
      (word
        '("der Kurs")
        '("IPA")
        '("kèchéng")
        '("42")
        '("课程"))
      (hash
        'description ""
        'tags (list (create-tag! "computer"))))
    (complex-word
      (word
        '("das Computerprogramm")
        '("IPA")
        '("chéngxù")
        '("24")
        '("程序"))
      (hash
        'description ""
        'tags (list (create-tag! "computer"))))
    (complex-word
      (word
        '("Data Science")
        '("IPA")
        '("shùjùkēxué")
        '("4412")
        '("数据科学"))
      (hash
        'description ""
        'tags (list (create-tag! "computer"))))
    (complex-word
      (word
        '("Machine Learning")
        '("IPA")
        '("???")
        '("???")
        '("???"))
      (hash
        'description ""
        'tags (list (create-tag! "computer"))))
    (complex-word
      (word
        '("Artificial Intelligence")
        '("IPA")
        '("???")
        '("???")
        '("???"))
      (hash
        'description ""
        'tags (list (create-tag! "computer"))))
    (complex-word
      (word
        '("sich fuer eine Person entscheiden")
        '("IPA")
        '("xuǎnzé")
        '("32")
        '("选择"))
      (hash
        'description ""
        'tags (list (create-tag! "politics"))))
    (complex-word
      (word
        '("teilnehmen")
        '("IPA")
        '("cānyù")
        '("14")
        '("参与"))
      (hash
        'description (string-append
                       "Im Gegensatz zu 参加 bedeutet 参与, dass man nur zu einem kleinen Teil teilnimmt. "
                       "Zum Beispiel bei einer Wahl. "
                       "Ein anderes Beispiel ist die Teilnahme an einem Musikkonzert."
                       "我参与那个音乐会。 - Ich habe auf dem Musikkonzert der Musik zuehoert."
                       "我参加那个音乐会。 - Ich habe auf dem Musikkonzert die Musik gespielt.")
        'tags (list (create-tag! "politics"))))
    (complex-word
      (word
        '("die Wahl")
        '("IPA")
        '("dàxuǎn")
        '("43")
        '("大选"))
      (hash
        'description ""
        'tags (list (create-tag! "politics"))))))
