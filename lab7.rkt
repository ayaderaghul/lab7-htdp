;; update the Writing Style (at the end of this file)
;; (say-something NEW-WS)
;;


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

(define DIALG
  (list
   (cons (list
          (cons "hi" (list (cons "," 1))))
         (list
          (cons "oh" (list (cons "hi" 1)))
          (cons "hi" (list (cons "," 1)))))
   (cons (list
          (cons "how" (list (cons "are" 1)
                            (cons "is" 1)))
          (cons "are" (list (cons "you?" 1)
                            (cons "things" 1)))
          (cons "you" (list (cons "?" 1))))
         (list
          (cons "i" (list (cons "am" 1)))
          (cons "am" (list (cons "okay" 1)))
          (cons "okay" (list (cons "." 1)))))))

(define (break-down sen)
  (update-ws '() sen))

;(define (similar-length? sen1 sen2)
;  (define l1 (length sen1))
;  (define l2 (length sen2))
;  (and (>= l2 (- l1 5))
;       (<= l2 (+ l1 5))))

;(define (add-to-ws* ws sen1 sen2)
;  (string-split sen1)
;  (string-split sen2)


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
     (foldr (lambda (w1 w2 ws) (add-to-ws ws w1 w2))
            ws (list word1d " ")
            (list "." word2)))]
   [(end? word2)
    (local
     [(define word2d (dedot word2))]
     (foldr (lambda (w1 w2 ws) (add-to-ws ws w1 w2))
           ws (list word1 word2d) (list word2d ".")))]
   [else (add-to-ws ws word1 word2)]))

(define (add-to-ws-in-reverse ws word1 word2)
  (cond
   [(end? word1) ; take them. -> them. take -> .them take
    (local
     [(define word1d (dedot word1))]
     (foldr (lambda (w1 w2 ws) (add-to-ws ws w1 w2))
            ws (list "." word1d)
            (list word1d word2)))]
   [(end? word2) ; end. The -> The end. -> The .end
    (local
     [(define word2d (dedot word2))]
     (foldr (lambda (w1 w2 ws) (add-to-ws ws w1 w2))
            ws (list word1 ".")
            (list " " word2d)))]
   [else (add-to-ws ws word1 word2)]))

(define (update-ws ws lst)
  (foldr (lambda (word1 word2 ws) (add-to-ws ws word1 word2))
         ws (drop-right lst 1) (rest lst)))

(define (update-ws2 ws lst)
  (foldr (lambda (word1 word2 ws) (add-to-ws2 ws word1 word2))
         ws (drop-right lst 1) (rest lst)))

(define (update-ws-in-reverse ws lst)
  (foldr (lambda (word1 word2 ws) (add-to-ws-in-reverse ws word1 word2))
         ws (drop-right lst 1)
         (rest lst)))


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

(define (previous-word ws word1)
  (cond
   [(equal? word1 " ") '()]
   [else
    (local
     [(define rule (get ws word1))
      (define word2 (grab-random rule))]
     (cons word2 (previous-word ws word2)))]))


(define (say-something ws)
  (string-join (next-word ws " ")))

(define (say-something-in-reverse ws)
  (string-join (reverse (previous-word ws "."))))

(require 2htdp/batch-io)

;(define dia (read-lines "dialogue"))



;(define game (read-words "game"))
;(define NEW (update-ws2 STYLE game))
(define input (read-words "sample2"))
;(define THORN (read-words "thorn"))
(define NEW (update-ws2 STYLE input))
(define NEWR (update-ws-in-reverse STYLE (reverse input)))
;(define NEW2 (update-ws NEW THORN))

(define (say-something-about keyword)
  (string-join
   (append
    (reverse (previous-word NEWR keyword))
    (list keyword)
    (next-word NEW keyword))))

;; version 2: markov machine but memory 2
