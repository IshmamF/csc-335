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

; Qestion 3

; pre: a binary tree
; post: a non negative integer

; Design Idea: We iterate through the tree, where if it's atom we recurse on the cdr of the list, otherwise recurse
; on both car and cdr of the list. We would have a counter that will be initially 0, and we will increment by 1
; on the recursion of car of the list.

; Guess Code:
#; (define (getHeight lat cntr)
     (cond (( check if list is empty ) return cntr)
           (( check if atom ) (simply recurse by doing cdr))
           (( else ( recurse on car and increment cntr by 1)))
           ))

; Code:
(define (getHeight lat cntr)
  (cond ((null? lat) cntr)
        ((atom? lat) cntr)
        (else
         (let ((left (getHeight (car lat) (+ 1 cntr)))
               (right (getHeight (cadr lat) (+ 1 cntr))))
          (cond
           ((> left right)left)
           (else right))))
        ))

