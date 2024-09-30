(define (atom? x)
  (not (list? x)))

; Question 1, Solution 1
(define (reverseNum x)
  (reverseNumHelper x 0))


(define (reverseNumHelper num curr)
  (cond
    ((= num 0) curr)
    (else (reverseNumHelper (quotient num 10) (+ (modulo num 10) (* curr 10))))
    ))

;--------------------------------------------------------------------------------------------------------
; Question 1, Solution 2

;Design Idea:
; To get the last digit of a number x, we do x modulo 10. We then multiply it with 10 to the power of number of digits - 1,
; since the last digit would now become the first. We add the value to the recursive call that will get
; x quotient 10 and  number of digits - 1 as arguments.
; We continue the iteration until we're at the first digit, which at that point, we can return it.
; We would need to 

; Pre: The program takes a integer >= 0 with no trailing 0s as an input
; Post: Returns the number of digits in the input - 1
(define (countPlace x)
  (cond
    ((< x 10) 0)
    (else (+ (countPlace (quotient x 10)) 1))))
; Inductive Hypothesis:
; If the pre is satisfied ahead of the recursive call, we can assume it works for inputs with 1 less digit.
; x >= 0 is an integer, so (quotient x 10) is also an integer >= 0.
; Base case: When x is 1 digit, we return 0. This is valid since although 1 digit should have 1 place,
; for our helper function (reverseNumHelper2), we need place - 1 since we have to do a number to the power of
; place value.
; Inductive Step:
; Provided the IH, and # of digits in (quotient x 10) is < # of digits in x , we can conclude that (countPlace (quotient x 10))
; returns the number of places - 1. Every iteration, we're incrementing the stack by 1 until we hit out base case.
; Termination: Program only stops when x has one digit aka less than 10. 

; Pre: The program takes a integer >= 0 with no trailing 0s as an input
; Post: Returns the positive integer reversed
(define (reverseNumHelper2 num place)
  (cond
    ((< num 10) num)
    (else (+ (* (modulo num 10) (expt 10 place)) (reverseNumHelper2 (quotient num 10) (- place 1))))))

; Pre: The program takes a integer >= 0 with no trailing 0s as an input
; Post: Returns the positive integer reversed
(define (reverseNum2 x)
  (reverseNumHelper2 x (countPlace x)))

; Inductive Hypothesis:
; If the pre is satisfied ahead of the recursive call, we can assume it works for inputs with 1 less digit.
; num >= 0 is an integer, so (quotient num 10) is also an integer >= 0.

; Base case: When num has one digit, we simply return it.

; Inductive Step: Provided the IH and # of digits in (quotient num 10) is < # of digits in num, we can conclude
; (reverseNumHelper2 (quotient num 10) (- place 1)) returns the first digits reversed. We get the remainder, multiply it
; by 10 to the power of place, and add it to the recursive call for each iteration.
; Termination: Program only stops when x has one digit aka less than 10.

;--------------------------------------------------------------------------------------------------------
; Question 2

; Specify:
; Pre - The input is a list (lat) of n atoms, where n is a number 0 or greater
; Post - a list with n length and the same input atoms, but in reverse order

; Design Idea:
; We use a helper function that takes in lat and an empty list (will hold processed), and iterates
; through the list, adding each atom to the processed list. When we "cons" an atom to a list, it get's
; added to the front of the list, so the first atoms would be near end of the list and vice versa

; Guess Code:
(define (reverseListHelper lat new)
  (cond
    ((null? lat) new)
    (else (reverseListHelper (cdr lat) (cons (car lat) new)))))

(define (reverseList lat)
  (reverseListHelper lat '()))

; Guess-Invariant:
; reverseListHelper(N) = reverseListHelper(# of processed atoms) + reverseListHelper(# of unprocessed atoms).
; Strong enough because when our processed list is empty, we return the result.
; Isn't weak enough because at the start we haven't processed any elements.

; Base Case:
; If lat is null, it should return new. The following holds because there are no more atoms to add
; to new and we've gone through the whole list, new holds the final resuit. If input is empty, we return an empty list.

; Inductive Hypothesis:
; If the pre is satisfied ahead of the recursive call, assume it works for the recursive call on the shorter list (cdr lat).
; Since lat has n elements >= 0, (cdr lat) is also a list with number of elements >= 0. In the case that lat is an empty
; list, we hit our base case.

; Inductive Step:
; On the first call, given our IH, we're provided lat which has n elements so at this point we know our result is right.
; At the end of the first call, if lat is not an empty list, which at that point we return an empty list since the
; reverse of an empty list is an empty list, we "con" the "car" of lat to new, and "cdr" lat as arguments for the next call. 
; The next call follows the same process, where we have n elements and our IH still holds. At the last call, where lat is empty,
; we return new since we have no more items to process. 

; Termination: When unprocessed list is empty, aka we've gone through the whole list, we can return the result (new)

;--------------------------------------------------------------------------------------------------------
; Qestion 3

; pre: a binary tree
; post: a non negative integer

; Design Idea: When looking at a tree, we make two decisions, go left or go right. We need to recurse through each path
; (left and right) to figure out which one has the larger depth. Then we can choose to go down the largerr path.
; We increment counter by 1 every time we go to the next node. If the list is empty or we hit an atom, we return counter.

; Guess Code:
#; (define (getHeight lat cntr)
     (cond (( check if list is empty ) return cntr)
           (( check if atom ) ( return cntr ))
           (( else ( if the left subtree has higher depth, go on left subtree, else go on right subtree)))
           ))

; Code:
(define (getHeight lat cntr)
  (cond ((null? lat) cntr)
        ((atom? lat) cntr)
        (else
         (let ((left (getHeight (car lat) (+ 1 cntr)))
               (right (getHeight (cadr lat) (+ 1 cntr))))
          (max left right)))
        ))

; Question 4

; Guess Code:
(define (reverse-tree lat)
     (cond ((null? lat) '())
           ((atom? lat) lat)
           (else
           (let ((rest (reverse-tree (cdr lat))))
             (cond
               ((atom? (car lat)) (cons (car lat) rest))
               (else (cons (reverse-tree (reverseList (car lat))) rest))
           )))))

(define (reverse-start lat)
  (if (atom? lat)
   lat
   (reverse-tree (reverseList lat))))

; Question 5

; Guess Code:
(define (count-occur t a)
  (cond
    ((null? t) 0)
    ((atom? t)
     (cond ((eq? t a) 1)
           (else 0)))
    ((atom? (car t))
     (cond
       ((eq? (car t) a) (+ 1 (count-occur (cdr t) a)))
       (else (count-occur (cdr t) a))))
    (else (+ (count-occur (car t) a) (count-occur (cdr t) a)))
    ))
                                     
