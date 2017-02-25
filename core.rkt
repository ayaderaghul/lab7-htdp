(require 2htdp/batch-io)
(require racket/hash)
(define input (read-file "sample.txt"))

(define (scan lst) ; unused yet
  (foldl (lambda (word h) (hash-update h word add1 0))
   (hash) lst))

(define data (remove-duplicates (string-split input)))

;; making the machine

(struct state (word dispatch) #:transparent)

(define (random-member lst)
  (list-ref lst (random (length lst))))

(define (make-sample-machine lst)
  (define l (length lst))
  (define (make-transition)
    (hash (round-2 (random)) (random-member lst)
          (round-2 (random)) (random-member lst)))
  (foldl (lambda (word h) (hash-union h (hash word (make-transition))))
         (hash) lst))

(define (round-n x n)
  (/ (round (* x (expt 10 n))) (expt 10 n)))
(define (round-2 x)
  (round-n x 2))

;; generate sentences

(define (accumulate lst)
  (define total (apply + lst))
  (let absolute->relative ([elements lst] [so-far #i0.0])
    (cond
     [(empty? elements) '()]
     [else (define nxt (+ so-far (round-2 (/ (first elements) total))))
           (cons nxt (absolute->relative (rest elements) nxt))])))

(define (randomise accumulated-lst)
  (define r (random))
  (for/last ([p (in-naturals)] [% (in-list accumulated-lst)] #:final (< r %)) p))

(define (generate-text m n)
  (define l (hash-count m))
  (define r (random l))
  (match-define (cons first-word dispatch) (hash-iterate-pair m r))
  (cons first-word
        (let generate ([count-down n] [next-batch dispatch])
          (cond
           [(zero? count-down) '()]
           [else
            (define proba (hash-keys dispatch))
            (define next-word (hash-iterate-key m (randomise (accumulate proba))))
            (define next-dispatch (hash-ref m next-word))
            (cons next-word (generate (- n 1) next-dispatch))]))))
