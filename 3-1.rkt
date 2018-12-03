#lang racket

(define (get-squares start-x start-y size-x size-y)
  (for/list ([x (in-range start-x (+ start-x size-x))]
             #:when #t
             [y (in-range start-y (+ start-y size-y))])
    (cons x y)))

(define/match (parse line)
  [((pregexp #px"^.*? @ (\\d+),(\\d+): (\\d+)x(\\d+)" (cons _ spec)))
     (map string->number spec)])

(define (process file)
  (define seen (make-hash))
  (for* ([line (in-lines file)]
         [square (apply get-squares (parse line))])
    (hash-update! seen square add1 0))
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
