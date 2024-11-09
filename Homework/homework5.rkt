#lang eopl

; Exercise 2.1 [ ] Implement the four required operations for bigits. Then use it to calculate the factorial of 10.
; How does the execution time vary as this argument changes? How does the execution time vary as the base
; changes? Explain why.

; Increment
(define inc
  (lambda (x)
    (+ x 1)))

; Decrement
(define dec
  (lambda (x)
    (- x 1)))

; Addition
(define add
  (lambda (x y)
    (cond ((= x 0) y)
          (else (inc (add (dec x) y))))))

; Subtraction
(define sub
  (lambda (x y)
    (cond ((= y 0) x)
          (else (dec (sub x (dec y)))))))

; Multiplication
(define mult
  (lambda (x y)
    (cond ((= y 0) 0)
          (else (add x (mult x (dec y)))))))

; Division
(define div
  (lambda (x y)
    (cond ((< x y) (list 0 x))
          (else (list (inc (car (div (sub x y) y))) (cadr (div (sub x y) y))))
          )))

; Factorial
(define fact
  (lambda (n)
    (cond ((= n 1) 1)
          (else (mult n (fact (dec n)))))))

; assume base-16
; (2 1) = 18
; (1 3) = 49
; 18 + 49 = 67
; (3 4)
; (15 1) = 31
; (2 3) = 50
; (17 4) = 81
; you cant have it over 16 though
; (1 5) have to increment the next value, decrement current by 16
; x = (15 15)
; y = (1 2)
; (0 2 1)
; 15 + 1 = 16
; 16 - 16 = 0
; x = (15)
; y = (1 2)
; 15 + 1 = 16
; If the next value isn't null, we add 1 to the next value in y
; if it is null, we cons 1

; add two bigits together 
(define b-add
  (lambda (x y)
    (cond ((null? x) y)
          ((null? y) x)
          (else (let* ((curX (car x))
                       (curY (car y))
                       (curr-sum (add curX curY))
                       (newX (remainder curr-sum 16))
                       (nextX (cdr x))
                       (nextY (cdr y)))
                (cond ((and (>= curr-sum 16) (null? nextY) (null? nextX))
                       (cons newX (cons 1 (b-add nextX nextY))))
                      
                       ((and (>= curr-sum 16) (null? nextY))
                       (cons newX (b-add (cons (inc (car nextX))(cdr nextX)) nextY)))
                       
                      ((and (>= curr-sum 16) (null? nextX))
                       (cons newX (b-add nextX (cons (inc (car nextY))(cdr nextY)))))
                      
                      ((>= curr-sum 16)(cons newX (b-add nextX (cons (inc (car nextY))(cdr nextY)))))
                      
                      (else (cons curr-sum (b-add nextX nextY)))))))))

(b-add (list 15 15) (list 1 2))

; are my succ and pred defined correctly?
; do we create a function for factorial?
; How do we create factorial function that uses these functions?
(define zero '())
(define iszero zero?)
(define succ (lambda (n) (quotient n 16)))
(define pred (lambda (n) (remainder n 16)))

(define bigit
  (lambda (n)
    (cond ((iszero n) zero)
          (else (cons (succ n) (bigit (pred n)))))))
; pre: a number
; post: a list of numbers that represent the number in a form

(define (myrep m)
  (letrec ((rep (lambda (n)
                  (cond ((iszero n) zero)
                        (else (let ((q (rep (quotient n m)))
                              (r (remainder n m)))
                          (cons r q)))))))
    rep))

; Actual Succ
; Bigit Increment
; What are all the cases?
; We increment a number between 0 and 14 by 1, which is very trivial
; We increment a number that's at 15 by 1, in which case we have to carry
; over to the next number, and current number becomes 0
; We increment a number at the end of a list and it's 15, we have to cons a 1 to the list
; We hit our base case of an empty list, function returns an empty list
(define b-inc
  (lambda (bigit)
    (cond ((null? bigit) (list 1))
          ((>= (car bigit) 15)
           (cons 0 (b-inc (cdr bigit))))
          (else (append (list (inc (car bigit))) (cdr bigit))))))


; Actual Pred
; Bigit Decrement
; What are the cases?
; When we have a 0 and decrement, the current number will be 15 and
; next number gets decremented
; We should check if the next number being decremented is 0, in which case
; we dont return it
(define b-dec
  (lambda (bigit)
    (cond ((null? bigit) '())
          ((and (= (length bigit) 1) (iszero (car bigit))) (list 0))
          ((iszero (car bigit))(let* ((next (cdr bigit)))
                                 (if (= (car next) 1)
                                     (if (null? (cdr next))
                                         (cons 15 (b-dec (cdr next)))
                                         (append (list 15 0)(cdr next)))
                                     (cons 15 (b-dec next)))))
          (else (append (list (dec (car bigit))) (cdr bigit))))))

; Operation #3
(define bigit-zero?
  (lambda (bigit)
    (and (= (length bigit) 1) (iszero (car bigit)))))
                                     

(define bAdd
  (lambda (x y)
    (cond ((bigit-zero? x) y)
          (else (b-inc (bAdd (b-dec x) y))))))

(define bSub
  (lambda (x y)
    (cond ((bigit-zero? y) x)
          (else (b-dec (bSub x (b-dec y)))))))


(define bMult
  (lambda (x y)
    (cond ((bigit-zero? y) (list 0))
          (else (bAdd x (bMult x (b-dec y)))))))


(define bFact
  (lambda (n)
    (cond ((equal? n (list 1)) (list 1))
          (else (bMult n (bFact (b-dec n)))))))


(define base15 (myrep 15))
(define base16 (myrep 16))

; Exercise 2.4 [ ] Implement a bintree-to-list procedure for binary trees, so that
; (bintree-to-list (interior-node 'a (leaf-node 3) (leaf-node 4)))
; returns the list (interior-node a (leaf-node 3) (leaf-node 4))

; BNF
; bintree ::= <number> | <symbol> <bintree> <bintree>

(define-datatype bintree bintree?
  (leaf-node (datum number?))
  (interior-node (key symbol?) (left bintree?) (right bintree?))
  )

(define bintree-to-list
  (lambda (tree)
    (cases bintree tree
      (leaf-node (datum) (begin
                           (display "leaf node: ")
                           (display datum)
                           (newline)
                           (list 'leaf-node datum)))
      (interior-node (key left right)
                     (list 'interior-node key (bintree-to-list left) (bintree-to-list right))))))


(define leaf-sum
  (lambda (tree)
    (cases bintree tree
      (leaf-node (datum) datum)
      (interior-node (key left right)
                     (+ (leaf-sum left)(leaf-sum right)))
      )))

(define tree (interior-node 'a (leaf-node 3) (leaf-node 4)))
(define tree3
  (interior-node 'd
    (interior-node 'e (leaf-node 8) (leaf-node 9))
    (interior-node 'f (leaf-node 10) (interior-node 'g (leaf-node 11) (leaf-node 12)))))

; Exercise 2.5 [ ] Use cases to write max-interior, which takes a binary tree of numbers
; with at least one interior node and returns the symbol associated with an interior node with a maximal leaf sum.
; > (define tree-a (interior-node 'a (leaf-node 2) (leaf-node 3)))
; > (define tree-b (interior-node 'b (leaf-node -1) tree-a))
; > (define tree-c (interior-node 'c tree-b (leaf-node 1)))
; > (max-interior tree-b)a> (max-interior tree-c)c
; The last invocation of max-interior might also have returned a, since both the a and c nodes
; have a leaf sum of 5.


; Professor's Design Idea:
; Create a list with pairs for each symbol, where
; you'd have the form (symbol max-sum)
; Use map to do cadr on the the list to get all the sums as a list
; Use accumulate to find the max of the list
; Reverse look up to find which symbol is associated with the max

; My Design Idea:
; we return a (symbol max-sum) at every recursive call, and
; we can expect our sub-trees to be evaluated. Then we compare
; the max-sum at each evaluated tree, and then return the list.
; There are multiple cases
; Our base case is that , both left and right are leaf-nodes
; at that point we just get the values of left and right,
; call them left-sum and right-sum, and return the sum and
; symbol at current level. 
; The next case(s) is whether left or right are leaf-nodes, but not
; both. In the case our left is a leaf-node, we want to do the sum of
; left-sum and the number
; within the list returned from (max-interior right) call it right-max,
; and compare it with right-max itself. If right-max is larger, we return
; (max-interior right), else we return a list with the current key and the sum
; we calculated. If right is a leaf-node, we do the same process but on the
; other way around.
; The last case is when both are interior nodes. In that case, we compare 
; left-max (number within list returned from (max-interior left) , right-max,
; and the sum of left-max and right-max. 

(define is-leaf-node
  (lambda (tree)
    (cases bintree tree
      (leaf-node (datum) #t)
      (interior-node (key left right)#f))))

; pre : List of lists that are of the form (symbol number)
; post : (symbol number) where number is the largest out of the three

(define find-max
  (lambda (input)
    (let* ((first (car input))
           (second (cadr input))
           (third (caddr input))
           (num1 (cadr first))
           (num2 (cadr second))
           (num3 (cadr third)))
      (cond ((and (>= num1 num2) (>= num1 num3)) first)
            ((and (>= num2 num1) (>= num2 num3)) second)
            (else third)))))


(define (max-interior tree)
  (letrec ((helper
  (lambda (tree)
    (cases bintree tree
      (interior-node (key left right)
                     (let* ((left-sum (leaf-sum left))
                            (right-sum (leaf-sum right)))
                       (cond 
                         ((and (is-leaf-node left) (is-leaf-node right))
                           (list key (+ left-sum right-sum)))
                         ((is-leaf-node left)(let* ((right-call (helper right))
                                                    (right-max (cadr right-call)))
                                               (
                                                    if (>= (+ right-max left-sum) right-max)
                                                 (list key (+ right-max left-sum))
                                                 right-call)))
                         ((is-leaf-node right)(let* ((left-call (helper left))
                                                     (left-max (cadr left-call)))
                                                (
                                                  if (>= (+ left-max right-sum) left-max)
                                                 (list key (+ left-max right-sum))
                                                 left-call)))
                         (else (let* ((left-call (helper left))
                                      (right-call (helper right))
                                      (left-max (cadr left-call))
                                      (right-max (cadr right-call)))
                                 (find-max (list key (+ left-max right-max))
                                     left-call
                                     right-call))))))
      (else 'ignore)))))
    (car (helper tree))))

(define tree-a (interior-node 'a (leaf-node 2) (leaf-node 3)))
(define tree-b (interior-node 'b (leaf-node -1) tree-a))
(define tree-c (interior-node 'c tree-b (leaf-node 1)))