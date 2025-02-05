#lang eopl

; Datatype Defintion

(define-datatype primitive primitive?
  (add-exp)
  (mult-exp))

(define-datatype expression expression?
  (var-exp (id symbol?))
  (lit-exp (datum number?))
  (if-exp (test-exp expression?) (true-exp expression?) (false-exp expression?))
  (lambda-exp (ids symbol?) (body expression?))
  (primapp-exp (prim primitive?) (rator expression?) (rand expression?))
  (app-exp (rator expression?) (rand expression?)))

; Exercise 2.10 [   ] Consider the definition of fresh-id:
(define fresh-id
  (lambda (exp s)
    (let ((syms (all-ids exp)))
      (letrec ((loop (lambda (n)
                       (let ((sym (string->symbol
                                   (string-append (symbol->string s)
                                                  (number->string n)))))
                         (if (memv sym syms)
                             (loop (+ n 1))
                             sym)))))
        (loop 0)))))

; Implement fresh-id by defining all-ids, which finds all the symbols in
; an expression. This includes the free occurrences, the bound occurrences,
; and the lambda identifiers for which there are no bound occurrences.


(define all-ids
  (lambda (exp)
    (cases expression exp
      (var-exp (id) (list id))
      (if-exp (test-exp true-exp false-exp) (union-set (all-ids test-exp)
                                                       (union-set (all-ids true-exp)
                                                                  (all-ids false-exp))))
      (lambda-exp (ids body) (union-set (list ids) (all-ids body)))
      (app-exp (rator rand) (union-set (all-ids rator) (all-ids rand)))
      (primapp-exp (prim rand1 rand2) (union-set (all-ids rand1) (all-ids rand2)))
      (else '()))))

(define union-set
  (lambda (l1 l2)
    (cond ((null? l1) l2)
          ((member (car l1) l2) (union-set (cdr l1) l2))
          (else (cons (car l1) (union-set (cdr l1) l2))))))

(define example1
  (lambda-exp
    'x
    (if-exp
      (app-exp 
        (var-exp 'f)
        (var-exp 's))
      (lambda-exp
        'z
        (app-exp
          (var-exp 'x)
          (var-exp 'z)))
      (var-exp 'y))))


(define freshid-ex (fresh-id (app-exp (lambda-exp 'w0 (app-exp (var-exp 'w1) (var-exp 'w2))) (var-exp 'w3)) 'w))


; Exercise 2.11 [     ] Let us assume that our lambda calculus expression has been enhanced with
; the constants 3, *, and +. Extend parse-expression and unparse-expression to support this enhancement.
; Fix lambda-calculus-subst so that it performs renaming when necessary.
; Hint: use fresh-id from the previous exercise.


;; Though Process:
;; The only time we need to rename is when there's a conflict where the variable
;; is both free and bounded in the same expression after subtitution. In which case
;; we should rename the bounded variable.
;; Do we only know if there's a conflict after the substitution ?
;; Im assuming at every lambda expression, we should use occurs bound
;; and occurs free, and do the intersection of both of these
;; to see if there are any conflicting variables...
;; Well actually you'd probably have to get all-ids, then check
;; each id if it occurs free & occurs bound at the same time...
;; Another issue, we can't change the substituted expression
;; and have to change the variable with lambda expression
;; and values before subst-exp.

(define lambda-calculus-subst
  (lambda (exp subst-exp subst-id)
    (letrec ((subst 
               (lambda (exp)
                 (cases expression exp
                   (var-exp (id)
                            (if (eqv? id subst-id)
                                subst-exp
                                exp))
                   (lambda-exp (id body)
                               (lambda-exp id (subst body)))
                   (app-exp (rator rand)
                            (app-exp (subst rator) (subst rand)))
                   (lit-exp (datum)
                            (lit-exp datum))
                   (if-exp (test-exp true-exp false-exp) (if-exp (subst test-exp)
                                                                 (subst true-exp)
                                                                 (subst false-exp)))
                   (primapp-exp (prim rand1 rand2)
                                (primapp-exp prim (subst rand1) (subst rand2)))
                   ))))
      (subst exp))))

             


; Exercise 2.24 [     ] Define a substitution to be a function whose domain is the set of Scheme symbols
; and whose range is the set of all terms (exercise 2.13). The interface for substitutions consists
; of (empty- subst), which binds its argument to a variable term of its argument, referred to as a
; trivial association (apply-subst s i), which returns the value of symbol i in substitution s
; and (extend-subst i t s), which returns a new substitution like s, except that symbol i is associated
; with term t.

; Implement the data type of substitutions with both a procedural representation and an
; abstract syntax tree representation. Then implement a procedure subst-in-term that
; takes a term and a substitution and walks through the term replacing each variable
; with its association in the substitution, much like the procedure subst of section 1.2.2.
; Finally, implement subst-in-terms that takes a list of terms.
