;problem set 6
;Terry Pireaux
;due 24 October 2017

"problem 1a"
;the function takes a function f and a list of integers A as inputs
;returns a list B containing the indexes of A for which f evalutates to #t
(define (remove-if f elements)
  (cond ((null? elements) '())
        ((f (car elements))
         (remove-if f (cdr elements)))
        (else (cons (car elements)
                     (remove-if f (cdr elements))))))
"test"
"(remove-if even? '())"
(remove-if even? '())
"(remove-if even? '(1 2 3 4 5 6))"
(remove-if even? '(1 2 3 4 5 6))
"(remove-if even? '(1 3 5 7 9))"
(remove-if even? '(1 3 5 7 9))

"problem 1b"
;function takes a thing to be removed v and list A
;removes a thing v from list A and returns a new list B
;must work with nested lists
(define (nested-remove v elements)
  (cond ((null? elements)'())
        ;begin checking for possible nested lists
        ((list? (car elements))
         ;check if v = first element before diving in case v is a list/pair
         (if (equal? v (car elements))
             ;move on to next index of elements if equal
             (nested-remove v (cdr elements))
             ;make new list if not a match
             (cons (nested-remove v (car elements))
                           (nested-remove v (cdr elements)))))
        ;move on to next index of elements if equal
        ((equal? v (car elements)) (nested-remove v (cdr elements)))
        ;make new list if not a match
        (else (cons (car elements) (nested-remove v (cdr elements))))))

"test"
"(nested-remove 2 '())"
(nested-remove 2 '())
"(nested-remove 2 '(1 2 3 4 5))"
(nested-remove 2 '(1 2 3 4 5))
"(nested-remove 2 '(1 2 '(1 2) 3 4 '(1 2)))"
(nested-remove 2 '(1 2 '(1 2) 3 4 '(1 2)))
"(nested-remove 2 '(1 3 4 5 6))"
(nested-remove 2 '(1 3 4 5 6))
"(nested-remove 'b '(b 2 (a b)))"
(nested-remove 'b '(b 2 (a b)))
"(nested-remove '(a b) '(1 2 (a b)))"
(nested-remove '(a b) '(1 2 (a b)))

"problem 2a"
;Convert any positive integer x to a list of single-digit integers
;12345 --> '(1 2 3 4 5)
(define (explode x)
  ;function to reverse the final list
  (define (reverse items)
     (define (reverse-iter r-items rest)
       (if (null? r-items)
           rest
           (reverse-iter (cdr r-items)
                         (cons (car r-items) rest))))
     (reverse-iter items '()))
  ;function to return digits in a list from LSB -> MSB
  (define (make-lst x)
    (if (= x 0)
        '()
        (cons (modulo x 10) (make-lst (floor (/ x 10))))))
  ;handle the 0 input and initiate the make lst if not
  (if (= x 0)
      '(0)
      (reverse (make-lst x))))
  
"test"
"(explode 0)"
(explode 0)
"(explode 1)"
(explode 1)
"(explode 1432)"
(explode 1432)
"(explode 103)"
(explode 103)
"(explode 1003)"
(explode 1003)
"(explode 12345)"
(explode 12345)

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
      ;raise the car to the power of its positon and sum it with recursion of the next MSB
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
;Step 1: 1458 -> (1 4 5 8) // Step 2: 1+4+5+8 = 18 // Step 3: 18 -> 18*81 = 1458
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
  (if (> x 0)
      (let* ((p (sum-l (explode x))) ;break x into a list then sum the indexes
             (q (implode (reverse (explode p)))));turn p into a list, reverse, and turn it back into an int
        (= (* p q) x))
      #f))
"test"
"(has-property 103)"
(has-property 103) 
"(has-property 1458)"
(has-property 1458)

"problem 2d"
;there are four #s that that have the Musahiko Fujiwara property
;the trvial case 1 and 1458 are two
;find the other 2

(define (sequence n)
  (* n 1))

(define (find sequence test n)
  (define (pass-accum a b k) ; a = iteration #, b = # test passes
    (cond ((= b k) (sequence (- a 1)))
          ((test (sequence a)) (pass-accum (+ a 1) (+ b 1) k))
          (else (pass-accum (+ a 1) b k))))
  (pass-accum 0 0 n))
"test"
"find the 2nd MF #, 81"
"(find sequence has-property 2)"
(find sequence has-property 2)
"find the 4th MF #, 1729"
"(find sequence has-property 4)"
(find sequence has-property 4)

"problem 3a"
;return #t if every member of a set B is in set A
(define (superset? a b)
  ;return #t if x is in set, else #f
  (define (element-of-set? x set)
    (cond ((null? set) #f)
          ((equal? x (car set)) #t)
          (else (element-of-set? x (cdr set )))))
  ;return n, the # of matches of B with A
  (define (set-match a b n)
    (cond ((null? b) n)
          ((element-of-set? (car b) a) (set-match (cdr b) a (+ n 1)))
          (else (set-match (cdr b) a n))))
  ;if the length of B is = # of matches of B to A then A is a superset of B
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
(define (cross-product a b)
  ;insert a separate scope so b can be iterated over without
  ;being void for the next position of a
  (define (combine a b)
    (if (null? b)
        '()
        (cons (list (car a) (car b)) (combine a (cdr b)))))
  (if (null? a)
      '()
      (append (combine a b) (cross-product (cdr a) b))))
"test"
"(cross-product '(1 2 3) '(4 5 6))"
(cross-product '(1 2 3) '(4 5 6))
  

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

"problem 5"
;take two lists of integers A and B as inputs
;assume each respective list is ordered small -> large
;return one sorted lists containing each index of A and B
(define (merge l1 l2)
  (cond ((and (null? l1) (null? l2)) '())
        ((null? l1) l2)
        ((null? l2) l1)
        ((>= (car l1) (car l2))
            (cons (car l2) (merge l1 (cdr l2))))
        (else (cons (car l1) (merge (cdr l1) l2)))))

"test"
"(merge '() '())"
(merge '() '())
"(merge '() '(2 3 4))"
(merge '() '(2 3 4))
"(merge '(1 2 3) '())"
(merge '(1 2 3) '())
"(merge '(1 2 3 4 5) '(4 5 6 7 8))"
(merge '(1 2 3 4 5) '(4 5 6 7 8))
        


          