#lang eopl

; 2.7
; Define the data type and parse and unparse procedures for the above grammar.
; Then implement lexical-address of exercise 1.31 using abstract syntax.
; It will be helpful to add two new variants representing the translation of a
; given bound or free variable reference. The value returned by lexical- address
; may then be generated using an unparse procedure that takes an abstract syntax
; tree of the form indicated by the above grammar, but with lex-info and free-info
; variants in place of var-exp variants.


(define-datatype expression expression?
  (lit-exp (datum number?))
  (var-exp (id symbol?))
  (lex-info   (id symbol?) (depth number?) (position number?))
  (free-info (id symbol?) (free symbol?))
  (if-exp (test-exp expression?) (true-exp expression?) (false-exp expression?))
  (lambda-exp (ids (list-of symbol?)) (body expression?))
  (app-exp (rator expression?) (rand expression?)))

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

(define example (lambda-exp (list 'a 'b) (var-exp 'a)))

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

(define (occurs-free? exp)
  (cases expression exp
    (var-exp (id) (list id))
    (lit-exp (datum) (list datum))
    (lambda-exp (ids body) (do stuff with ids and (recursive call on body)))
    (if-exp (test-exp true-exp false-exp) (union-set on recursive calls))
    (app-exp (rator rand) (union-set on recursive calls))
    