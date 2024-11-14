#lang racket
(require racket/set)

; Write a procedure free-vars that takes a list structure representing an
; expression in the lambda calculus syntax given above and returns a set
; (a list without duplicates) of all the variables that occur free in the
; expression. Similarly, write a procedure bound-vars that returns a set of
; all the variables that occur bound in its argument

; BNF
; <expression> ::== identifier
;              ::== (lambda (identifier) (<expression>)
;              ::== (<expression><expression>)

; Data structure:

; helpers:
(define first
  (lambda (exp)
    (car exp)))
(define second
  (lambda (exp)
    (cadr exp)))
(define third
  (lambda (exp)
    (caddr exp)))

; pre: a list representing an expression and an atom
; post: an updated expression with all occurances of the atom removed

; IH: Assuming the pre holds, the next call works
; base case: if the list is empty, then we return an empty list
; IS: Given our IH and (cdr exp) is a subproblem,
; (equal? x (car exp)) (removeVar x (cdr exp))
; holds because if we reach an element that matches our
; input x, we ensure our sub-problem no longer has the current element.
; We know (cons (car exp) (removeVar x (cdr exp)))
; holds because we still want the elements that don't match our input x.

(define removeVar
  (lambda (x exp)
    (cond ((null? exp) '())
          ((equal? x (car exp)) (removeVar x (cdr exp)))
          (else (cons (car exp) (removeVar x (cdr exp))))
          )))

; pre: two sets of variables
; post: a set that is the union of the two input sets
; IH: Assuming our pre holds, the next call works
; Base Case: If the first list is empty, we return the second list
; IS: Given our IH and (cdr lst1) is a simpler subproblem,
; (member (car lst1) lst2) (union-set (cdr lst1) lst2)
; holds because if the current item is in the second list, we ignore
; the element to make sure we don't count it twice since it's already in
; the second list. 
; (cons (car lst1) (union-set (cdr lst1) lst2))
; holds because if it's not in list 2, we want the current element
; since a union has all elements from each list without duplicates.
(define union-set
  (lambda (lst1 lst2)
    (cond ((null? lst1)lst2)
          ((member (car lst1) lst2) (union-set (cdr lst1) lst2))
          (else (cons (car lst1) (union-set (cdr lst1) lst2)))
          )))

; constructors:
(define make-lambda
  (lambda (var body)
    (list 'lambda (list var) body)))

; selectors:
(define get-var
  (lambda (exp)
    (first (second exp))))

(define get-body
  (lambda (exp)
    (third exp)))
; classifiers:
(define is-lambda?
  (lambda (exp)
    (eq? (first exp) 'lambda)))

; pre: list representing an expression in the lambda calculus
; post: a set of all the variables that occur free in the
; expression

; (`lambda (`x) (`lambda (`y) `x `+ `y))
(define free-vars
  (lambda (exp)
    (cond ((symbol? exp) (list exp))
          ((is-lambda? exp) (removeVar (get-var exp)
                                    (free-vars (get-body exp))))
          (else (union-set (free-vars (first exp))
                           (free-vars (second exp))))
          )))


; Structural Induction:

; IH: Assuming the recursive call works on subexpression of exp,
; our subproblems will return a set of free variables

; Base Case: If we get a symbol, we simply return it in a list
; since it's not bounded by anything, so it's trivially free.

; Case 1 (lambda (identifier) (body))
; If the expression is a lambda expression, the recursive call then processes the
; body of the expression.
; By the IH, (remove (get-var exp)(free-vars (get-body exp)))
; holds because we remove all instances of the bounded variable (get-var exp)
; from our recursive call (free-vars (get-body exp)) ensuring we get a set
; of free variables.

; Case 2 (expression expression)
; Otherwise, we recursively process the subproblems
; (free-vars (first exp)) and (free-vars (second exp))
; which are each just expressions without `lambda.
; By the IH, the recursive call
; (set-union (free-vars (first exp))(free-vars (second exp))
; holds because each subproblem returns a set of free variables
; and ensures that the union of both sets has no
; duplicates, giving the correct intended expression.

; Termination:
; There can only be a finite number of sub-expressions and
; since we're always taking the car, cadr, or caddr, going through
; each sub-expression, we're guaranteed to reach a symbol, which
; the program then returns ensuring a termination.

#;(define free-vars
  (lambda (exp)
    (cond ((symbol? exp) 
           (display "Symbol: ") 
           (display exp) 
           (newline)  ; Print the symbol
           (list exp))
          ((is-lambda? exp) 
           (display "Lambda Expression: ") 
           (display exp) 
           (newline)  ; Print the lambda expression
           (let ((vars (remove (get-var exp) (free-vars (get-body exp)))))
             (display "Free vars after lambda: ") 
             (display vars) 
             (newline)  ; Print free vars after lambda
             vars))
          (else 
           (display "Expression: ") 
           (display exp) 
           (newline)  ; Print the expression
           (let ((first-vars (free-vars (first exp)))
                 (second-vars (free-vars (second exp))))
             (display "First free vars: ") 
             (display first-vars) 
             (newline)  ; Print first free vars
             (display "Second free vars: ") 
             (display second-vars) 
             (newline)  ; Print second free vars
             (let ((result (union-set first-vars second-vars)))
               (display "Union of free vars: ") 
               (display result) 
               (newline)  ; Print union of free vars
               result))))))

