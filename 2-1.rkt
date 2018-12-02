#lang racket

(define twos 0)
(define (add-two!) (set! twos (add1 twos)))
(define threes 0)
(define (add-three!) (set! threes (add1 threes)))

(define (count string)
  (define has-two #f)
  (define has-three #f)
  
  (for-each
   (lambda (group)
     (cond
       [(eq? 2 (length group)) (set! has-two #t)]
       [(eq? 3 (length group)) (set! has-three #t)]))
   (group-by
    (lambda (x) (char->integer x))
    (string->list string)))
  
  (and has-two (add-two!))
  (and has-three (add-three!)))

(define (process in)
  (for ([line (in-lines in)])
    (count line)))

(call-with-input-file* "inp/2.txt"
  (lambda (file) (process file)))

(cons 'twos twos)
(cons 'threes threes)

(cons 'hash (* twos threes))