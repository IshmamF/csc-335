; Design Idea:
; As we recurse through the input, we will hold on to a list of all parameters, called "env".
; If we're given just a symbol, we check for it's depth and position in the "env". If the
; symbol doesn't exist in the "env" then it's a free variable, and we return a list
; with the variable and 'free'. If we get a depth and position, we return a list with
; the variable, colon, depth, and position in that order.  
; If it's an expression with 'lambda we 'cons' the parameters onto "env", and recurse on the
; body. If the expression has an 'if in it, we recurse down the two subexpressions. Else, we
; can have a list of any number of sub-expressions, in which case, we have to use a map
; helper function to do the recursive call on each sub-expression. 

; Data structure :

; Helper Functions:

; pre: a list of lists (lst) and a function that handles each list within the list (f)
; post: the same list of lists , but the function applied to each list
(define mymap
  (lambda (f lst)
    (cond ((null? lst) '())
          (else (cons (f (car lst)) (map f (cdr lst)))))))

; pre: expects a symbol (var), list of lists (env), and a number (depth)
; post: a list with the format var : d p or var 'free
; where d is depth and p is position 
(define getDP
  (lambda (var env depth)
    (cond ((null? env) (list var 'free))
          ((member var (car env)) (list var ': depth (getPos var (car env))))
          (else (getDP var (cdr env) (+ depth 1))))))

; pre: expects a symbol (var) and a list of atoms (lst)
; post: returns a number
(define getPos
  (lambda (var lst)
    (cond ((eq? var (car lst)) 0)
          (else (+ 1 (getPos var (cdr lst))))
          )))

; Constructors

(define make-lambda
  (lambda (a b)
    (list 'lambda a b)))

(define make-if
  (lambda (a b)
    (list 'if a b)))

; Selectors

(define first
  (lambda (exp)
    (car exp)))

(define second
  (lambda (exp)
    (cadr exp)))

(define third
  (lambda (exp)
    (caddr exp)))

; Classifiers

(define check-lambda
  (lambda (exp)
    (eq? (first exp) 'lambda)))

(define check-if
  (lambda (exp)
    (eq? (first exp) 'if)))

; Pre: The input is an expression that matches the bnf
; Post: The output is the same expression, except each variable is within a list that
; states it's depth and position (or free if it's a free variable)

; Main Program

(define lexical-scope
  (lambda (exp)
    (define helper
      (lambda (env exp)
        (if (symbol? exp)
            (getDP exp env 0)
            (let* ((add-params (cons (second exp) env)))
              (cond
                ((check-lambda exp) (make-lambda (second exp) (helper add-params (third exp))))
                ((check-if exp)(make-if (helper env (second exp)) (helper env (third exp))))
                (else (mymap (lambda (exp) (helper env exp)) exp))
                )))))
    (helper '() exp)))
        