; Question 1, Solution 1
(define (reverseNum x)
  (reverseNumHelper x 0))


(define (reverseNumHelper num curr)
  (cond
    ((= num 0) curr)
    (else (reverseNumHelper (quotient num 10) (+ (modulo num 10) (* curr 10))))
    ))

; Question 1, Solution 2
(define (countPlace x)
  (cond
    ((< x 10) 0)
    (else (+ (countPlace (quotient x 10)) 1))))

(define (reverseNumHelper2 num place)
  (cond
    ((< num 10) num)
    (else (+ (* (modulo num 10) (expt 10 place)) (reverseNumHelper2 (quotient num 10) (- place 1))))))


(define (reverseNum2 x)
  (reverseNumHelper2 x (countPlace x)))


; Question 2 Solution 
(define (reverseListHelper lat new)
  (cond
    ((null? lat) new)
    (else (reverseListHelper (cdr lat) (cons (car lat) new)))))

(define (reverseList lat)
  (reverseListHelper lat '()))