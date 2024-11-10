; Regular Fibonacci Sequence
#; (define (fib n)
  (cond ((zero? n) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))(fib (- n 2))))
        ))


; Okay this is just wrong lol
#; (define (fib2 n)
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

; I am so lost actually :(
 #;(define (fib m)
   (letrec ((helper1 (lambda (n)
                      (cond ((= n 0) 0)
                            ((= n 1) 1)
                            (else (extend-env (list n)
                                              (list (+ (helper1 (- n 1))
                                                       (helper1 (- n 2)) (- n 2)))
                                              empty-env)))))
            )
     (apply-env (lambda (x) (helper1 x)) m)))



; EOPL Functions
(define extend-env
  (lambda (keys vals env)
    (lambda (sym) (let ((pos (list-find-position sym keys)))
                    (if (number? pos)
                        (list-ref vals pos)
                        (apply-env env sym))))))

(define apply-env
  (lambda (env sym) (env sym)))

(define list-find-position
  (lambda (sym los) (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

(define list-index
  (lambda (pred ls)
    (cond ((null? ls) #f)
          ((pred (car ls)) 0)
          (else (let ((list-index-r (list-index pred (cdr ls))))
                  (if (number? list-index-r)
                      (+ list-index-r 1)
                      #f))))))

(define empty-env (lambda (sym) #f))


; This is still not using memoization effectively...
#; (define (fib-memo n env)
  (let ((cached-value (apply-env env n)))
    (if (number? cached-value)
        cached-value
        (let* ((v1 (fib-memo (- n 1) env))
               (env1 (extend-env (list (- n 1)) (list v1) env))
               (v2 (fib-memo (- n 2) env1))
               (env2 (extend-env (list (- n 2)) (list v2) env1))
               (result (+ v1 v2))
               (new-env (extend-env (list n) (list result) env2)))
          result))))

#;(define (fib n)
  (let ((env (extend-env (list 0 1) (list 0 1) empty-env)))
    (fib-memo n env)))


(define memo-fib
  (lambda (n env)
    (let* ((value (apply-env env n)))
      (cond ((= n 0) 0)
            ((= n 1) 1)
            ((number? value) value)
            (else (let* ((ans (+ (memo-fib (- n 1) env)(memo-fib (- n 2) env)))
                         (newEnv (extend-env (list n) (list ans) env)))
                    (apply-env newEnv n)))))))

(define newFib
  (lambda (n)
    (memo-fib n empty-env)))



         