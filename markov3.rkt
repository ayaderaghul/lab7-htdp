;; update the Writing Style (at the end of this file)
;; (say-something NEW-WS)
;;

;; version 3: markov machine memory 3


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
  (list (cons (list "i" "am" "here") (list (cons "great" 1) (cons "i" 1)))
        (cons (list "how" "is" "that") (list (cons "am" 1) (cons "are" 1)))))

(define (string-upcase? str)
  (char-upper-case? (first (string->list str))))

(define (add-to-ws ws word1 word2 word3 word4)
  (define (update-values current-val)
    (update current-val word4 add1 1))
  (update ws (list word1 word2 word3) update-values (list (cons word4 1))))

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

(define (add-to-ws2 ws word1 word2 word3 word4)
  (cond
   [(end? word1)
    (local
     [(define word1d (dedot word1))]
     (foldl (lambda (w1 w2 w3 w4 ws) (add-to-ws ws w1 w2 w3 w4))
            ws
            (list word1d "." " ")
            (list "." " " word2)
            (list " " word2 word3)
            (list word2 word3 word4)))]
   [(end? word2)
    (local
     [(define word2d (dedot word2))]
     (foldr (lambda (w1 w2 w3 w4 ws) (add-to-ws ws w1 w2 w3 w4))
           ws
           (list word1 word2d ".")
           (list word2d "." " ")
           (list "." " " word3)
           (list " " word3 word4)))]
   [(end? word3)
    (local
     [(define word3d (dedot word3))]
     (foldr (lambda (w1 w2 w3 w4 ws) (add-to-ws ws w1 w2 w3 w4))
            ws
            (list word1 word2 word3)
            (list word2 word3 ".")
            (list word3 "." " ")
            (list "." " " word4)))]
   [else (add-to-ws ws word1 word2 word3 word4)]))

(define (update-ws ws lst)
  (foldr (lambda (word1 word2 word3 word4 ws)
           (add-to-ws ws word1 word2 word3 word4))
         ws
         (drop-right lst 3)
         (drop-right (drop lst 1) 2)
         (drop-right (drop lst 2) 1)
         (drop lst 3)))

(define (update-ws2 ws lst)
  (foldr (lambda (word1 word2 word3 word4 ws)
           (add-to-ws2 ws word1 word2 word3 word4))
         ws
         (drop-right lst 3)
         (drop-right (drop lst 1) 2)
         (drop-right (drop lst 2) 1)
         (drop lst 3)))


(require "csv.rkt")
(define (out-data filename data)
  (define out (open-output-file filename #:mode 'text #:exists 'append))
  (write-table data out)
  (close-output-port out))

;(define (say-something ws)
  (define (next-word ws word1 word2 word3)
    (cond
     [(equal? word3 ".") '()]
     [else
      (local
       [(define rule (get ws (list word1 word2 word3)))
        (define word4 (grab-random rule))]
      ; (out-data "test" (list (list word2)))
       (cons word4 (next-word ws word2 word3 word4)))]))
; (next-word ws " "))

;(define (say-something ws)
;  (string-join (next-word ws "." " ")))

(define (begin-sentence? x)
  (equal? (rest x) (list "." " ")))


(require 2htdp/batch-io)

;(define dia (read-lines "dialogue"))



;(define game (read-words "game"))
;(define NEW (update-ws2 STYLE game))
;(define input (read-words "sample2"))
(define THORN (read-words "thorn"))
;(define NEW2 (update-ws2 STYLE input))
(define NEW (update-ws2 STYLE THORN))

(define sentence-opener
  (filter begin-sentence? (map first NEW)))

(define (random-mem x)
  (list-ref x (random (length x))))

;(define ice-breaker
;  (random-mem sentence-opener))

(define (say-something ws)
  (define ice-breaker
    (random-mem sentence-opener))
  (string-join (apply next-word
                      (append (cons ws ice-breaker)))))






;; version 2: markov machine but memory 2
