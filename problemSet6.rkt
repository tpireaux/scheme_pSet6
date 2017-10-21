;problem set 6
;Terry Pireaux
;due 24 October 2017

"problem 1a"
;
"test"

"problem 1a"
;
"test"

"problem 2a"
;Convert any positive integer x to a list of single-digit integers
;12345 --> '(1 2 3 4 5)
(define (explode x)
  (define (chip y n)
    (if (>= y 10)
        (chip (floor (/ y 10)) (+ n 1))
        (cons y n)))
  (if (= x 0)
      '()
      (let ((p (chip x 0)))
        (cons (car p) (explode (- x (* (car p) (expt 10 (cdr p)))))))))
"test"
"(explode 1)"
(explode 1)
"(explode 1432)"
(explode 1432)

"problem 2b"
;function takes in a list of base 10 digits and converts them to one integer
;'(1 2 3 4 5) --> 12345
;