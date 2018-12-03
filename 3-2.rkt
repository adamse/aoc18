#lang racket

(define (tiles start-x start-y size-x size-y)
  (remove-duplicates
   (for/list ([x (in-range start-x (+ start-x size-x))]
              #:when #t
              [y (in-range start-y (+ start-y size-y))])
     (cons x y))))

(define/match (parse line)
  [((pregexp #px"^#(\\d+) @ (\\d+),(\\d+): (\\d+)x(\\d+)" (cons _ spec)))
     (map string->number spec)])

(define (process file)
  (define claims
    (map parse (sequence->list (in-lines file))))

  (define stakes (make-hash))

  (for* ([claim (in-list claims)]
         [tile (in-list (apply tiles (cdr claim)))])
    (hash-update! stakes tile add1 0)) ;; why 0 here? I'm confused...

  (define (check tile)
    (eq? 1 (hash-ref stakes tile)))

  (define (check-claim claim)
    (foldr (lambda (tile acc) (and (check tile) acc))
           #t
           (apply tiles (cdr claim))))

  (for/list ([claim (in-list claims)]
        #:when (check-claim claim)
        #:final #t)
    (car claim)))

(define test
  "#1 @ 1,3: 4x4
#2 @ 3,1: 4x4
#3 @ 5,5: 2x2
")

(process (open-input-string test))
(call-with-input-file* "inp/3.txt" process)
