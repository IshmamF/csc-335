#lang eopl

; 2.7
; Define the data type and parse and unparse procedures for the above grammar.
; Then implement lexical-address of exercise 1.31 using abstract syntax.
; It will be helpful to add two new variants representing the translation of a
; given bound or free variable reference. The value returned by lexical- address
; may then be generated using an unparse procedure that takes an abstract syntax
; tree of the form indicated by the above grammar, but with lex-info and free-info
; variants in place of var-exp variants.

; Datatype Defintion
(define-datatype expression expression?
  (lit-exp (datum number?))
  (var-exp (id symbol?))
  (lex-info   (id symbol?) (depth number?) (position number?))
  (free-info (id symbol?) (free symbol?))
  (if-exp (test-exp expression?) (true-exp expression?) (false-exp expression?))
  (lambda-exp (ids (list-of symbol?)) (body expression?))
  (app-exp (rator expression?) (rand expression?)))

; Parsing an Expression and Converting to List
(define (parse-expression exp)
  (cases expression exp
    (lex-info (id depth position) (list id ': depth position))
    (free-info (id free) (list id free))
    (lit-exp (datum) datum)
    (var-exp (id) id)
    (if-exp (test-exp true-exp false-exp) (list 'if (parse-expression test-exp)
                                                (parse-expression true-exp)
                                                (parse-expression false-exp)))
    (lambda-exp (ids body) (list 'lambda ids (parse-expression body)))
    (app-exp (rator rand) (list (parse-expression rator) (parse-expression rand)))))

; Examples
(define example (lambda-exp (list 'a 'b) (var-exp 'a)))

(define example1
  (lambda-exp
    (list 'x 'y)
    (if-exp
      (app-exp 
        (var-exp 'f)
        (lit-exp 42))
      (lambda-exp
        (list 'z)
        (app-exp
          (var-exp 'x)
          (var-exp 'z)))
      (var-exp 'y))))


; Parsing an expression represented as a list and converting it into abstract syntax
(define (unparse-expression exp)
  (cond ((symbol? exp) (var-exp exp))
        ((number? exp) (lit-exp exp))
        ((and (= (length exp) 4) (eq? (cadr exp) ':)) (lex-info (car exp) (caddr exp) (cadddr exp)))
        ((and (= (length exp) 2) (eq? (cadr exp) 'free)) (free-info (car exp) (cadr exp)))
        ((eq? (car exp) 'lambda) (lambda-exp (cadr exp) (unparse-expression (caddr exp))))
        ((eq? (car exp) 'if) (if-exp (unparse-expression (cadr exp))
                                     (unparse-expression (caddr exp))
                                     (unparse-expression (cadddr exp))))
        (else (app-exp (unparse-expression (car exp)) (unparse-expression (cadr exp)) ))
        ))

; Gets lexical address within an expression
(define (lexical-address exp env)
  (cases expression exp
    (lit-exp (datum) (if (isFree? datum env)
                      (free-info datum 'free)
                      (lex-info datum (get-depth datum env) (get-position datum env))))
    (var-exp (id) (if (isFree? id env)
                      (free-info id 'free)
                      (lex-info id (get-depth id env) (get-position id env))))
    (if-exp (test-exp true-exp false-exp) (if-exp (lexical-address test-exp env)
                                                (lexical-address true-exp env)
                                                (lexical-address false-exp env)))
    (lambda-exp (ids body) (lambda-exp ids (lexical-address body (cons ids env))))
    (app-exp (rator rand) (app-exp (lexical-address rator env) (lexical-address rand env)))
    (else exp)))

(define (isFree? id env)
  (cond ((null? env) #t)
        ((member id (car env)) #f)
        (else isFree? id (cdr env))))

(define (get-depth id env)
  (cond ((member id (car env)) 0)
        (+ 1 (get-depth id (cdr env)))))

(define (get-position id env)
  (cond
        ((eq? id (car env)) 0)
        ((and (list? (car env)) (member id (car env))) (get-position id (car env)))
        (else (if (list? (car env))
                  (get-position id (cdr env))
                  (+ 1 (get-position id (cdr env)))))))

; 2.8 - Re-write exercise 1.19 to use abstract syntax tree
; Exercise 1.19 [     ] Write a procedure free-vars that takes a list structure representing
; an expression in the lambda calculus syntax given above and returns a set (a list without
; duplicates) of all the variables that occur free in the expression. Similarly, write a procedure
; bound-vars that returns a set of all the variables that occur bound in its argument.

; Occurs Free Implementation
(define (occurs-free? exp)
  (cases expression exp
    (var-exp (id) (list id))
    (lit-exp (datum) (list datum))
    (lex-info (id depth position) '())
    (free-info (id free) (list id))
    (lambda-exp (ids body) (remove-ids ids (occurs-free? body)))
    (if-exp (test-exp true-exp false-exp) (union-set (occurs-free? test-exp)
                                          (union-set (occurs-free? true-exp)
                                                     (occurs-free? false-exp))))
    (app-exp (rator rand) (union-set (occurs-free? rator)
                                     (occurs-free? rand)))
    ))

(define union-set
  (lambda (l1 l2)
    (cond ((null? l1) l2)
          ((member (car l1) l2) (union-set (cdr l1) l2))
          (else (cons (car l1) (union-set (cdr l1) l2))))))

(define remove-ids
  (lambda (ids body)
    (cond
    ((null? ids) body)
    (else (remove-ids (cdr ids) (remove (car ids) body))))))

(define accumulate
  (lambda (op seq init)
    (cond ((null? seq) init)
          (else (op (car seq) (accumulate op (cdr seq) init))))))

(define remove
  (lambda (id body)
    (accumulate (lambda (x y) (if (eq? id x)
                                  y
                                  (cons x y))) body '())))

; Occurs Bound Implementation

; Professor Way or Idea from Office Hour
; I think there's a bug with this since you're doing the union-set of all the variables possible
; with the lambda parameters that get called within the free variables.
#; (define (occurs-bound? exp)
  (cases expression exp
    (var-exp (id) (list id))
    (lit-exp (datum) (list datum))
    (lex-info (id depth position) (list id))
    (free-info (id free) '())
    (lambda-exp (ids body) (union-set (occurs-bound? body)
                                      (elements-of ids (occurs-free? body))))
    (if-exp (test-exp true-exp false-exp) (union-set (occurs-bound? test-exp)
                                          (union-set (occurs-bound? true-exp)
                                                     (occurs-bound? false-exp))))
    (app-exp (rator rand) (union-set (occurs-bound? rator)
                                     (occurs-bound? rand)))
    ))

(define elements-of
  (lambda (ids lst)
    (cond ((null? ids) '())
          ((member (car ids) lst) (cons (car ids) (elements-of (cdr ids) lst)))
          (else (elements-of (cdr ids) lst)))))

; My Way
(define (occurs-bound? exp)
  (define (helper exp)
    (cases expression exp
      (var-exp (id) (list id))
      (lit-exp (datum) (list datum))
      (lex-info (id depth position) (list id))
      (free-info (id free) '())
      (lambda-exp (ids body) (helper body))
      (if-exp (test-exp true-exp false-exp) (union-set (helper test-exp)
                                                       (union-set (helper true-exp)
                                                                  (helper false-exp))))
      (app-exp (rator rand) (union-set (helper rator)
                                       (helper rand)))
      ))
  (filter-vars (helper exp) (occurs-free? exp)))


(define filter-vars
  (lambda (ids freeVars)
    (accumulate (lambda (x y) (if (member x freeVars)
                                  y
                                  (cons x y))) ids '())))

; Using intersect set fails because it only keeps the variable at the top of the expression
(define intersect-set
  (lambda (lst1 lst2)
    (cond ((null? lst1) '())
          ((null? lst2) '())
          ((member (car lst1) lst2) (cons (car lst1)
                                          (intersect-set (cdr lst1) lst2)))
          (else (intersect-set (cdr lst1) lst2)))))