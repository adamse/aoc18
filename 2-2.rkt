#lang racket

(define (string-diff s1 s2)
  (for/sum ([c1 (in-string s1)]
            [c2 (in-string s2)]
            #:unless (char=? c1 c2))
    1))

(define (common s1 s2)
  (list->string
   (for/list ([c1 (in-string s1)]
              [c2 (in-string s2)]
              #:when (char=? c1 c2))
     c1)))

(define (process in)
  (define lines (sequence->list (in-lines in)))
  (for/list
      ([s1 (in-list lines)]
       #:when #t
       [s2 (in-list lines)]
       #:when (eq? 1 (string-diff s1 s2))
       #:final #t)
    (common s1 s2)))

(define test
  "abcde
fghij
klmno
pqrst
fguij
axcye
wvxyz")

(process (open-input-string test))

(call-with-input-file* "inp/2.txt" process)

