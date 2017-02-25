(define sample (list (cons 'green 2) (cons 'red 5) ))
(define (get lst x)
  (cond
    [(empty? lst) #false]
    [else
     (if (equal? x (car (first lst)))
         (cdr (first lst))
         (get (rest lst) x))]))

(define (update lst x updater backup)
  (cond
    [(empty? lst) (cons (cons x backup) lst)]
    [else
     (if
      (equal? x (car (first lst)))
      (cons (cons x (updater (cdr (first lst)))) (rest lst))
      (cons (first lst) (update (rest lst) x updater backup)))]))

(define (add-to-counter lst x)
  (update lst x add1 1))

(define (total-size1 lst)
  (apply + (map cdr lst)))
(define (total-size lst)
  (local
    [(define (add pair base)
       (+ (cdr pair) base))]
    (foldr add 0 lst)))

(define (count lst x)
  (local
   [(define (x? n)
      (equal? x n))]
   (length (filter x? lst))))

(define (expected-counts lst n)
  (define total (total-size lst))
  (define (expected i)
    (* (/ (cdr i) total) n))
  (map expected lst))

(define (count-grabs counter lst)
  (cond
   [(empty? counter) '()]
   [else
    (cons
     (count lst (car (first counter)))
     (count-grabs (rest counter) lst))]))

(define (grab-random c)
  (local
   [(define (grab n c)
      (cond
       [(< n (cdr (first c))) (car (first c))]
       [else (grab (- n (cdr (first c)))
                   (rest c))]))]
   (grab (random (total-size c)) c)))

(define (grab-n counter n)
  (build-list n (lambda (_) (grab-random counter))))

(require test-engine/racket-tests)
(check-within (count-grabs MARBLE-BAG (grab-n MARBLE-BAG 10000))
              (expected-counts MARBLE-BAG 10000)
              100)

(define STYLE
  (list (cons "am" (list (cons "great" 1) (cons "i" 1)))
        (cons "how" (list (cons "am" 1) (cons "are" 1)))))

(define (string-upcase? str)
  (char-upper-case? (first (string->list str))))

(define (add-to-ws ws word1 word2)
  (define (update-values current-val)
    (update current-val word2 add1 1))
  (update ws word1 update-values (list (cons word2 1))))


(define (end? word)
  (equal? #\. (last (string->list word))))
(define (dedot word)
  (define l (string-length word))
  (substring word 0 (- l 1)))

(define (add-to-ws2 ws word1 word2)
  (cond
   [(end? word1)
    (local
     [(define word1d (dedot word1))]
     (foldl (lambda (w1 w2 ws) (add-to-ws ws w1 w2))
            ws (list word1d " ")
            (list "." word2)))]
   [(end? word2)
    (local
     [(define word2d (dedot word2))]
     (foldr (lambda (w1 w2 ws) (add-to-ws ws w1 w2))
           ws (list word1 word2d) (list word2d ".")))]
   [else (add-to-ws ws word1 word2)]))

(define (update-ws ws lst)
  (foldr (lambda (word1 word2 ws) (add-to-ws ws word1 word2))
         ws (drop-right lst 1) (rest lst)))

(define (update-ws2 ws lst)
  (foldr (lambda (word1 word2 ws) (add-to-ws2 ws word1 word2))
         ws (drop-right lst 1) (rest lst)))


(require "csv.rkt")
(define (out-data filename data)
  (define out (open-output-file filename #:mode 'text #:exists 'append))
  (write-table data out)
  (close-output-port out))

;(define (say-something ws)
  (define (next-word ws word1)
    (cond
     [(equal? word1 ".") '()]
     [else
      (local
       [(define rule (get ws word1))
        (define word2 (grab-random rule))]
      ; (out-data "test" (list (list word2)))
       (cons word2 (next-word ws word2)))]))
; (next-word ws " "))

(define (say-something ws)
  (string-join (next-word ws " ")))

(require 2htdp/batch-io)
(define input (read-words "sample2"))
(define THORN (read-words "thorn"))
(define NEW (update-ws2 STYLE input))
(define NEW2 (update-ws NEW THORN))
