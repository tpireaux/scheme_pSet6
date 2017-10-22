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
  ;function to reverse the final list
  (define (reverse-l items)
     (define (reverse-iter r-items rest)
       (if (null? r-items)
           rest
           (reverse-iter (cdr r-items)
                         (cons (car r-items) rest))))
     (reverse-iter items '()))
  ;function to return digits in a list for LSB -> MSB
  (define (make-lst x)
    (if (= x 0)
        '()
        (cons (modulo x 10) (make-lst (floor (/ x 10))))))
  (reverse-l (make-lst x)))
  
"test"
"(explode 1)"
(explode 1)
"(explode 1432)"
(explode 1432)
"(explode 103)"
(explode 103)

"problem 2b"
;function takes in a list of base 10 digits and converts them to one integer
;'(1 2 3 4 5) --> 12345
(define (implode l)
  (define (length-l l) ;function to return the length of string l as an int
    (define (length-iter l n) ; function takes in l and iterates until empty, counting the iterations
      (if (null? l)
          (+ n 0)
          (length-iter (cdr l) (+ n 1))))
    (length-iter l 0))
  (if (null? l)
      0
      (+ (* (car l)
            (expt 10 (- (length-l l) 1)))
         (implode (cdr l)))))
"test"
"(implode '(1 2 3 4 5))"
(implode '(1 2 3 4 5))
"(implode '())"
(implode '())
"(implode '(1 0 3))"
(implode '(1 0 3))

"problem 2c"
;function accepts an integer x and returns #t if x has the Musahiko Fujiwara property
;a number n that can be broken down into the sum of its digits
;the sum of its digits multiplied by the reverse of its digits is equal to n
;1458 -> 1+4+5+8 = 18 -> 18*81 = 1458
(define (has-property x)
  (define (sum-l l)
    (if (null? l)
        0
        (+ (car l) (sum-l (cdr l)))))
   (define (reverse items)
     (define (reverse-iter r-items rest)
       (if (null? r-items)
           rest
           (reverse-iter (cdr r-items)
                         (cons (car r-items) rest))))
     (reverse-iter items '()))
  (let* ((p (sum-l (explode x)))
         (q (implode (reverse (explode p)))))
    (= (* p q) x)))
"test"
"(has-property 103)"
(has-property 103) 
"(has-property 1458)"
(has-property 1458)

"problem 3a"
;return #t if every member of a set B is in set A
(define (superset? a b)
  (define (element-of-set? x set)
    (cond ((null? set) #f)
          ((equal? x (car set)) #t)
          (else (element-of-set? x (cdr set )))))
  (define (set-match a b n)
    (cond ((null? b) n)
          ((element-of-set? (car b) a) (set-match (cdr b) a (+ n 1)))
          (else (set-match (cdr b) a n))))    
  (cond ((> (length b) (length a)) #f)
        ((and (null? a) (null? b)) #f)
        ((= (set-match a b 0) (length b)))))
"test"
"(superset? '() '())"
(superset? '() '())
"(superset? '() '(1 2 3))"
(superset? '() '(1 2 3))
"(superset? '(1 2 3) '(1 2 3))"
(superset? '(1 2 3) '(1 2 3))

"problem 3b"
;return the differce between set A and B
;that is a - b = {x : (x ∈ a ∧ x !∈ b)}
(define (set-difference a b)
  (define (element-of-set? x set)
    (cond ((null? set) #f)
          ((equal? x (car set)) #t)
          (else (element-of-set? x (cdr set )))))
  (cond ((null? a) '())
        ((null? b) a)
        ((if (element-of-set? (car a) b)
             (set-difference (cdr a) b)
             (cons (car a) (set-difference (cdr a) b))))))

"test"
"(set-difference '() '())"
(set-difference '() '())
"(set-difference '(1 2 3) '())"
(set-difference '(1 2 3) '())
"(set-difference '(1 2 3) '(1 2 3))"
(set-difference '(1 2 3) '(1 2 3))
"(set-difference '(1 2 3) '(1 2 3 4 5 6 7))"
(set-difference '(1 2 3) '(1 2 3 4 5 6 7))
"(set-difference '(1 3 5 7 9) '(1 2 3 4 5 6 7 8 9))"
(set-difference '(1 3 5 7 9) '(1 2 4 5 6 7))

"problem 3c"
;return set of tuples (x, y) such that x is in a and y is in b

"problem 4"
;takes a list as input and returns a list
;if the input list is comprised of nested list the contents will be unested in the output
(define (nestless lst)
  (cond ((null? lst) '())
        ((list? (car lst)) (append (nestless (car lst))
                                   (nestless (cdr lst))))
        (else (cons (car lst) (nestless (cdr lst))))))
"test"
"(nestless '())"
(nestless '())
"(nestless '(1 2 3 4 5))"
(nestless '(1 2 3 4 5))
"(nestless '(1 2 '(3 4 5) 6 7 8))"
(nestless '(1 2 '(3 4 5) 6 7 8))
  

          