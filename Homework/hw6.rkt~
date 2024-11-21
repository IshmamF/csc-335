#lang eopl

; Datatype Defintion
(define-datatype expression expression?
  (lit-exp (datum number?))
  (var-exp (id symbol?))
  (if-exp (test-exp expression?) (true-exp expression?) (false-exp expression?))
  (lambda-exp (ids (list-of symbol?)) (body expression?))
  (app-exp (rator expression?) (rand expression?)))

; Exercise 2.10 [   ] Consider the definition of fresh-id:
(define fresh-id
  (lambda (exp s)
    (let ((syms (all-ids exp)))
      (letrec ((loop (lambda (n)
                       (let ((sym (string->symbol
                                   (string-append s
                                                  (number->string n)))))
                         (if (memv sym syms)
                             (loop (+ n 1))
                             sym)))))
        (loop 0)))))

(define all-ids
  (lambda (exp)
    (cases expression exp
      (lit-exp (datum) (list datum))
      (var-exp (id) (list id))
      (if-exp (test-exp true-exp false-exp) (union-set (all-ids test-exp)
                                                       (union-set (all-ids true-exp)
                                                                  (all-ids false-exp))))
      (lambda-exp (ids body) (union-set ids (all-ids body)))
      (app-exp (rator rand) (union-set (all-ids rator) (all-ids rand))))))

(define union-set
  (lambda (l1 l2)
    (cond ((null? l1) l2)
          ((member (car l1) l2) (union-set (cdr l1) l2))
          (else (cons (car l1) (union-set (cdr l1) l2))))))

(define example1
  (lambda-exp
    (list 'x 'y)
    (if-exp
      (app-exp 
        (var-exp 'f)
        (lit-exp 42))
      (lambda-exp
        (list 'z 'd)
        (app-exp
          (var-exp 'x)
          (var-exp 'z)))
      (var-exp 'y))))



