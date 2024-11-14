; Regular Fibonacci Sequence
 (define (fib0 n)
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
; Can probably optimize extend-env by not using lists, and just have key and val
; would remove the need to find position and list-ref 
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

<<<<<<< HEAD
=======

>>>>>>> 1ee3985 (updated)
; Updated empty-env to return #f 
(define empty-env (lambda (sym) #f))

; experimenting with looking up
(define env1 (extend-env '(a b c) '(1 2 3) empty-env))
(define env2 (extend-env '(f e) '(4 5) env1))
(define myenv3 

; This works, and it uses 2.3 material but runtime is the same as regular fib function
(define (fib n)
  (define (helper n env)
    (let ((result (apply-env env n)))
      (if (number? result)
          result
          (let ((new-result (+ (helper (- n 1) env)
                               (helper (- n 2) env))))
            (apply-env (extend-env (list n) (list new-result) env) n)))))
  (helper n env3))


(define env3
  (extend-env (list 0 1) (list 0 1) empty-env))

; Chatgpt Assisted Solution
; The reason this works is because we're not just returning the result
; We also return the environment associated with the result which
; has the new value attached to it for our recursive calls
<<<<<<< HEAD
; Also once we can use the environment returned from the doing the first
; recursive call, which is what allows us to have extended environments
; Its a lot to think about to be honest, but understand that we return
; the environment instead of just the result is very important.
; Then the using environment from (helper (n-1) env) part allows us to
; not compute the same solution again. That's what we want to happen since
; the things that are computed on the left (n - 1) would need to be
; computed again on the right (n - 2). Everything else, is consistent with
; my previous solution above.
=======
; https://www.figma.com/board/Owa4cGKHvUMKypQY3dsEoz/Untitled?node-id=0-1&t=3eyy1VZNEhhA55qk-1
>>>>>>> 1ee3985 (updated)
(define (fib n)
  (define (helper n env)
    (let ((result (apply-env env n))) ; checking the table
      (if (number? result)
          (cons result env) 
          (letrec ((result1 (helper (- n 1) env))
                 (fib1 (car result1))
                 (env1 (cdr result1))
                 (result2 (helper (- n 2) env1))
                 (fib2 (car result2))
                 (env2 (cdr result2))
                 (new-fib (+ fib1 fib2))
                 (new-env (extend-env (list n) (list new-fib) env2)))
            (cons new-fib new-env)))))
  (car (helper n env3)))

(define env3
  (extend-env (list 0 1) (list 0 1) empty-env))

<<<<<<< HEAD
; Figma Drawing on how the function works
; https://www.figma.com/board/Owa4cGKHvUMKypQY3dsEoz/Untitled?node-id=0-1&t=3eyy1VZNEhhA55qk-1

; Alternative Solution using a list
(define helper
  (lambda (n memo)
    (if (list? (assv n memo))
        (list (cadr (assv n memo)) memo) ; can create our own implementation of assv for apply-env
        (let* ((result1 (helper (- n 1) memo))
               (val1 (car result1))
               (memo1 (cadr result1))
               (result2 (helper (- n 2) memo1)) ; you need to get/use the updated memo from result1
               (val2 (car result2))
               (memo2 (cadr result2))
               (newVal (+ val1 val2))
               (newMemo (cons (list n newVal) memo2))) ; can probably change the extend-env to do this simple task
          (list newVal newMemo)))))
(define fib2
  (lambda (n)
    (car (helper n `((0 1)(1 1)(2 1))))))
        
=======

         
>>>>>>> 1ee3985 (updated)
