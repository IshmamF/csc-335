#lang eopl

; 1. **Implement `greater?` and `equal?` Primitives**

;To add `greater?` and `equal?` as primitives:

;### Modify the `primitive` datatype:
; Add two new constructors for `greater?-prim` and `equal?-prim`:

(define-datatype primitive primitive?
  (mult-prim)
  (add-prim)
  (subtract-prim)
  (incr-prim)
  (decr-prim)
  (greater?-prim)
  (equal?-prim))


;### Extend `apply-primitive`:
;Handle the new primitives in the `apply-primitive` function:

(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim () (+ (car args) (cadr args)))
      (subtract-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1))
      (greater?-prim () (if (> (car args) (cadr args)) 1 0)) ; True → 1, False → 0
      (equal?-prim () (if (= (car args) (cadr args)) 1 0))))) ; True → 1, False → 0


; A `cond` expression evaluates a series of test-expressions and returns the result of the first true branch. To incorporate this:

; ### Extend the `expression` datatype:
; Add a new constructor for `cond-exp`:

(define-datatype expression expression?
  (lit-exp (datum number?))
  (var-exp (id symbol?))
  (primapp-exp (prim primitive?) (rands (list-of expression?)))
  (cond-exp (clauses (list-of clause?)))) ; New


;### Define a `clause` datatype:
;Each `clause` has a test and a corresponding expression:

(define-datatype clause clause?
  (clause-form (test expression?) (result expression?)))




;## 3. **Adjust `eval-expression` for `cond-exp`**

;Add a new case to handle the `cond-exp` expression:

(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim args)))
      (cond-exp (clauses)
                (eval-cond clauses env))))) ; New case for cond

;### Define `eval-cond`:
;Iterate over the clauses and evaluate each test. Return the result of the first true test:

(define eval-cond
  (lambda (clauses env)
    (if (null? clauses)
        (eopl:error 'eval-cond "No clause in cond was true.")
        (cases clause (car clauses)
          (clause-form (test result)
            (if (true-value? (eval-expression test env))
                (eval-expression result env)
                (eval-cond (cdr clauses) env)))))))


;## 4. **Adjust `cond` Syntax in User Programs**

;### Example `cond` Syntax:
;A `cond` expression might look like this:

(cond-exp
  (list
    (clause (primapp-exp (greater?-prim) (list (var-exp 'i) (lit-exp 1)))
            (lit-exp 'z))
    (clause (primapp-exp (equal?-prim) (list (var-exp 'x) (lit-exp 10)))
            (lit-exp 18))))

;### Example Program:

(a-program
  (cond-exp
    (list
      (clause (primapp-exp (greater?-prim) (list (var-exp 'i) (lit-exp 1))) 
              (lit-exp 'z))
      (clause (primapp-exp (equal?-prim) (list (var-exp 'x) (lit-exp 10))) 
              (lit-exp 18)))))

