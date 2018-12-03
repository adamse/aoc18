#lang racket

(define (get-squares start-x start-y size-x size-y)
  (for/list ([x (in-range start-x (+ start-x size-x))]
             #:when #t
             [y (in-range start-y (+ start-y size-y))])
    (cons x y)))

(define (parse line) ;; truly horrible...
  (let*
      ([inp (string-split line)]
       [start-s (string-split (car (cdr (cdr inp))) ",")]
       
       [start-x (string->number (car start-s))]
       [start-y (string->number (string-trim (car (cdr start-s)) ":"))]
                                 
       [size-s (string-split (car (cdr (cdr (cdr inp)))) "x")]
       [size-x (string->number (car size-s))]
       [size-y (string->number (car (cdr size-s)))])
    (list start-x start-y size-x size-y)))

(define (process file)
  (define seen (make-hash))
  (for* ([line (in-lines file)]
         [square (apply get-squares (parse line))])
    (hash-update! seen square (lambda (x) (add1 x)) 0))
  (for/sum ([x (in-list (hash->list seen))]
            #:unless (<= (cdr x) 1))
    1))

(define test
  "#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2
")

(process (open-input-string test))
(call-with-input-file* "inp/3.txt" process)
