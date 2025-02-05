#lang eopl

;(define repl read-eval-print)

;(cond-exp (test-exps conseq exp)

#;(let-exp (ids rands body)
         (let ((args (eval-let-exprands rands env)))
           (eval-expression body (extendenv ids args env))))

;; let-exp's last line reads as evaluating the expression's body in an enviornment that's been extended by the let bindings.
;; Does this correctly implement let?

(define scheme-value?
  (lambda (v) #t))

(define-datatype program program?
  (a-program (pgm expression?)))

(define-datatype primitive primitive?
  (mult-prim)
  (add-prim)
  (subtract-prim)
  (incr-prim)
  (decr-prim))

(define-datatype expression expression?
  (lit-exp (datum number?))
  (var-exp (id symbol?))
  (primapp-exp (prim primitive?) (rands (list-of expression?))))

(define-datatype environment environment?
  (empty-env)
  (extend-env (ids (list-of symbol?))
              (vals (list-of scheme-value?))
              (env environment?)))

(define empty-environment
  (lambda () empty-env))

(define extend-environment
  (lambda (ids vals env)
    (extend-env ids vals env)))

(define find-list-index
  (lambda (ids sym)
    (letrec ((helper (lambda (ids sym index)
                       (cond ((null? ids) #f)
                             ((eq? (car ids) sym) index)
                             (else (helper (cdr ids) sym (+ index 1)))))))
      (helper ids sym 0))))

(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env () (eopl:error 'apply-env "No binding for ~s" sym))
      (extend-env (ids vals env)
                  (let ((index (find-list-index ids sym)))
                    (if (number? index)
                        (list-ref vals index)
                        (apply-env env sym)))))))
                      


(define true-value?
  (lambda (x) (not (zero? x))))

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
                 (eval-expression body (init-env))))))



(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum)
      (var-exp (id) (apply-env env id))
      (primapp-exp (prim rands)
                   (let ((args (eval-rands rands env)))
                     (apply-primitive prim args))))))

(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim () (+ (car args) (cadr args)))
      (subtract-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1)))))

(define init-env
  (lambda ()
    (extend-env '(i v x) '(1 5 10) (empty-env))))

;cond greater? (i,1) ==> z
;equal? (x,10) ==> 18
;end

;i is 1, v is 5, , and x is 10, and emptylist is ()

;cond greater? (i,1) ==> z
;     equal? (x,10) ==> 18
;end

;; How could I adjust the cond syntax to work with primapp-exp?
;; Could I have cond with no expressions?
;; Do I need cond-aux or is there a nicer way of doing it?

;; each operand is evaluated in the same enviornment
;; key idea of implementing procedures. How do you implement procedures? You evaluate the body in an enviorment that has been extending from the formals to the actuals.

;; Quiz question: implement let*
;; Quiz question: add an else clause to cond, argue that it's right, and test it.
;; Quiz question: how would you implement a multi-argument equal?
;; Final Exam Question: Figure out what fold-left looks like, implemented? Figure out what accumulate looks like, implemented?
;;; you'd want it to return #t with multiple-same value arguments. 
;; Inducitively, the accumulated results so far would either be true or false if everything has been equal so far
;;; Related question: How would you fold a vector?

;; In scheme, (equal? 1 1) returns #t, but supposed you do (equal? 1 1 1)? You'll get an error. What if you did (= 1 1 1)? it'll return #t. So