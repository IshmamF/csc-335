(define (fib n)
  (cond ((zero? n) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))(fib (- n 2))))
        ))

(define (fib2 n)
  (letrec ((env '())
           (extend-env (lambda (x y)
                         (cons (list x y) env)))
           (check-env (lambda (x env)
                        (cond ((null? env)#f)
                              ((member x (car env))#t)
                              (else (check-env x (cdr env))))))
           (apply-env (lambda (x env)
                         (cond ((member x (car env)) (cadr (car env)))
                               (else (apply-env x (cdr env)))))))
    (cond ((zero? n) (extend-env 0 0))
          ((= n 1) (extend-env 1 1))
          ((and (check-env (- n 2) env)(check-env (- n 1) env))
           (+ (apply-env (- n 2) env)(apply-env (- n 1) env)))
          (else (+ (fib (- n 1))(fib (- n 2)))))))



         