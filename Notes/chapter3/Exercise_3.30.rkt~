#lang eopl

(define scanner-spec-3-1
  '((white-sp
     (whitespace)                                    skip)
    (comment
     ("%" (arbno (not #\newline)))                   skip)
    (identifier
     (letter (arbno (or letter digit "?")))          symbol)
    (number
     (digit (arbno digit))                           number)
    ))

(define grammar-3-19
  '((program
     (expression) a-program)
    (expression
     (number)
     lit-exp)
    (expression
     (identifier)
     var-exp)
    (expression
     (primitive "(" (separated-list expression ",")  ")" )
     primapp-exp)
    (expression
     ("if" arbno expression "then" expression "else" expression)
     if-exp)
    (expression
     ("cond" (arbno expression "==>" expression ) "end")
     cond-exp)
    (expression
     ("let" (arbno expression "=" expression ) "in" expression)
     let-exp)
    (primitive ("+")
               add-prim)
    (primitive ("-")
               subtract-prim)
    (primitive ("*")
               mult-prim)
    (primitive ("add1")
              incr-prim)
    (primitive ("sub1")
               decr-prim)
    (primitive ("minus")
               (minus-prim))
    (primitive ("cons")
               (cons-prim))
    (primitive ("car")
               (car-prim))
    (primitive ("cdr")
               (cdr-prim))
    (primitive ("list")
               (list-prim))
    (primitive ("eq?")
               eq-prim)
    (primitive ("equal?")
               equal-prim)
    (primitive ("zero?")
               zero-prim)
    (primitive ("greater?")
               greater-prim)
    (primitive ("less?")
               less-prim)
    (primitive ("null?")
               null-prim)
    ))

(define repl read-eval-print)

(cond-exp (test-exps conseq exp)

(let-exp (ids rands body)
         (let ((args (eval-let-exprands rands env)))
           (eval-expression body (extendenv ids args env))))

;; let-exp's last line reads as evaluating the expression's body in an enviornment that's been extended by the let bindings.
;; Does this correctly implement let?

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