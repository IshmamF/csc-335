; increments a number input by 1 and returns that new value
(define (inc x)
  (+ x 1))

; decrements a number input by 1 and returns that new value
(define (dec x)
  (- x 1))

;--------------------------------------------------------------------------------------------------------

; precondition x and y are positive integers
; post condition the sum of x and y
(define (myAdd x y)
  (cond
    ((zero? y) x)
    (else (inc (myAdd x (dec y))))))

;--------------------------------------------------------------------------------------------------------

; precondition x is a positive integer
; post condition the sum of all the digits in x

; Design Idea:
; Get the remainder of x divided by 10, where x is a non negative integer. Add that to the call stack, and
; recurse by passing the quotient of x and 10. When we reach the last integer, we would just return it since
; it's less than 10 and don't need to modulo it - we also don't have anymore digits to add.

; Example:
; 123 mod 10 is 12 and we get remainder 3, 12 mod 10 is 1 remainder 2, we then return 1 so 1 + 2 + 3 = 6

; IH: If the precondition holds ahead of the recursive call, we can assume it works for inputs with 1 less digit.
; Since x is a non-negative integer, the floor of x divided 10 should also be an non-negative integer.

; base case is when the number is less than 10

; IS: Provided our IH and we know that the number of digits in the quotient of x divided by 10 is smaller than
; the number of digits in x, we can conclude that (sum-digits (floor (/ x 10))) returns the sum of the left most digits.
; We add (remainder x 10) on to the next recursive call until we x is less than 10, aka theres 1 digit left, in which case
; we return x, and sum up all the remainders we added onto the recursive stack.

; Guess Code
(define (sum-digits x)
  (cond
    ((< x 10) x)
    (else (+ (remainder x 10) (sum-digits (floor (/ x 10)))))))

; Termination: A non negative integer can be divided by 10 only a finite number of times before it becomes one digit
; which at that point the program stops.

;--------------------------------------------------------------------------------------------------------

; precondition l is a list
; post condition is an integer value that represents how many items are in the l
(define (myLength l)
  (cond
    ((null? l) 0)
    (else (inc (myLength (cdr l))))
    ))

;--------------------------------------------------------------------------------------------------------

; precondition l is a list and n is a positive integer
; post condition return l with it's first n values, if n is greater than the length of the list
; it returns l 
(define (first-n l n)
  (cond
    ((null? l) (quote()))
    ((zero? n) (quote()))
    (else (cons (car l) (first-n (cdr l) (dec n))))))

;--------------------------------------------------------------------------------------------------------

; precondition x and y are both non-empty lists
; post condition return a list with elements from both x and y
(define (myAppend x y)
  (cond
    ((and (null? x) (null? y)) (quote()))
    ((not (null? x)) (cons (car x) (myAppend (cdr x) y))) 
    (else (cons (car y) (myAppend x (cdr y))))
    ))
    

; Try your hand at modifying this program:

; 1. to input x and integer k >= 0, and which returns x without the kth occurance of '()
; - flawed spec, there can be less than k items of empty list, there can be less than k values
; 2. to input x and return the index of the first occurance of '() in x

; pre there are at least k empty lists
; post returns original list without the kth empty list
(define (kth-empty x k)
  (cond
    ((and (eq? k 1)(null? (car x))(cdr x)))
    ((null? (car x)) (cons (car x) (kth-empty (cdr x) (dec k))))
    (else (cons (car x) (kth-empty (cdr x) k)))
    ))

;--------------------------------------------------------------------------------------------------------

; Design Idea:
; Iterate through the list, incrementing each call by 1 until we hit an empty list. Once we hit empty list,
; we return 0, and combine all the 1's we've added.

; pre program takes a list with an empty list as an element for input
; post returns an integer value that is the index of the first occurance of an empty list

; Inductive Hypothesis:
; If the pre holds ahead of the recursive call, assume the call on the shorter list (cdr x) works as well.
; Since x is a list with at least one occurance of '(), we can assume that it's subset (cdr x) also has
; a '() as an element.

; Base Case: We get a list with the only element being '(). Since there's only one element, the return value
; is 0. 

; Inductive Step:
; Given our IH and the number of elements in (cdr x) is less than the number of elements in x,
; (first-occur (cdr x)) should return an integer value that holds the position of '() relative to
; the recursive call being made. When it hits the base case and returns to the earliest call, we
; increment by 1 each time.

; Termination: We stop iterating when we have found a '() as an element. Since the number of elements in a list
; is finite, and we are guaranteed at least one '() as an element, the program finishes when we hit our base case
; aka found the first occurance of an empty list - which at that point we return the value. 
(define (first-occur x)
  (cond
    ((null? (car x)) 0)
    (else (inc (first-occur (cdr x))))
    ))





