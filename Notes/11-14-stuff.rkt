;; e1: (lambda (p) (+ p x)) p is bounded
;; consider the expression resulting from substituting (* p 3)
;; for x.
;; f1: (lambda (p)(+ p (* p 3)))
;; Here the p in  (* p 3) has been captured by the (lambda (p))

;; e2: (lambda (q) (+ p x))
;; same here
;; f2: (lambda (q) (+ p (* p 3))) p is free

;; Are they logically identical?
;; No
;; Because the p in the lambda function with q is from somewhere
;; else and not bounded from the original lambda function.

;; A model for an expression e: an interpretation of the symbols
;; occuring in e.

;; Seeking a model which distinguishes f1 from f2
;; -------
;; Let's take + to denote addition, and * to denote multiplication.
;; -------
;; and 3 denotes 3
;; For these expressions, the only other parts open to interpretation
;; are the free variables. 

;; Model of e1: + is +
;;              p is any number
;;              x is a number

;; By substituting (* p 3) broke the model because you need to
;; define p else where , and f1 doesn't care about that p
;; which leads both functions to return different values

;; How to fix this? How to define substitutions so that meanings
;; are preserved?

;; Problem: p in (* p 3) is free in f1 but it has been
;; captured by the binder.

;; Change informal q to r
;; (lambda (r) (+ r (* p 3))


#lang eopl

(define-datatype primitive primitive?
  (add-prim)
  (mul-prim))

(define-datatype expression expression?
  (lit-exp
   (datum number?))
  (var-exp
    (id symbol?))
  (lambda-exp (id symbol?)
              (body expression?))
  (primapp-exp (prim primitive?)
               (rand1 expression?)
               (rand2 expression?))
  (app-exp (rator expression?)
           (rand expression?)))

(define lambda-calculus-subst
  (lambda (exp subst-exp subst-id)
    (letrec ((subst (lambda (exp)
                      (cases expression exp
                        (var-exp (id)
                                 (if (eqv? id subst-id) subst-exp exp))
                        (lambda-exp (id body)
                                    (lambda-exp id (subst body)))
                        (app-exp (rator rand)
                                 (app-exp (subst rator) (subst rand)))
                        (lit-exp (datum)
                                 (lit-exp datum))
                        (primapp-exp (prim rand1 rand2)
                                     (primapp-exp prim (subst rand1) (subst rand2)))
                        ))))
      (subst exp))))

(define example (lambda-exp 'a (var-exp 'a)))
(define subst-example (lambda-exp 'r (var-exp 'r)))

;; e1 : (lambda (p) (+ p x))
;; To make e1 suitable for input to lambda-calculus-subst we need to parse it.
;; Proceed one part at a time.

;; Concrete:                 Abstract (code to deliver abstract syntax)
;; +                         (add-prim)
;; p                         (var-exp 'p)
;; x                         (var-exp 'x)
;; (+ p x)                   (primapp-exp (add-prim) (var-exp 'p) (var-exp 'x))
;; (lambda (p) (+ p x))      (lambda-exp 'p (primapp-exp (add-prim)
;;                                                    (var-exp 'p)
;;                                                    (var-exp 'x)))

;; Quiz question can be parsing an expression or unparsing an expression
