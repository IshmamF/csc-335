;; Disclaimer: He did not give us our finals back so unfortunately I don't know
;;             if the following answers are correct or not. I'm also providing
;;             the questions and my answers from memory. You are expected to prove
;;             for each question. Unfortunately, I do not have the bandwidth to
;;             prove these questions again but refer to my previous proofs to get an idea!

#lang eopl

;; QUESTION 1:

;; Given a positive integer `n`, which represents the number of opening
;; and closing parentheses, write a function that generates a list of all
;; possible valid combinations of `n` pairs of parentheses.

;; 0 represents open parenthesis, 1 represents closing
;;
;; A valid parentheses combination follows the recursive grammar:
;; valid ::= ()
;;         | ( valid )
;;         | valid valid
;;
;; Example:
;; (generate-parentheses 6)
;; ;; Expected output:
;; ;; ("((()))"
;; ;;  "(()())"
;; ;;  "(())()"
;; ;;  "()(())"
;; ;;  "()()()")

;; MY ANSWER:

;; if a = '(0 1 0 1) , the function returns the following: '(0 a 1)
;; It encloses the input list with an opening and closing parenthesis
(define (surround lst)
  (let* ((add-0 (cons 0 lst)))
    (append add-0 '(1))))

;; if a = '(0 0 1 1), the function returns the following: '(0 1 a)
;; It adds an opening and closing parenthesis to the left of a
(define (concat-left lst)
  (append '(0 1) lst))

;; if a = '(0 0 1 1), the function returns the following: '(0 1 a)
;; It adds an opening and closing parenthesis to the right of a
(define (concat-right lst)
  (append lst '(0 1)))

;; the following function remove the duplicates for combinations
;; since it's possible to get the same combination from doing
;; any of the above functions. 
(define (remove-dups lst)
  (cond ((null? lst) '())
        ((member (car lst) (cdr lst)) (remove-dups (cdr lst)))
        (else (cons (car lst) (remove-dups (cdr lst))))))

;; this function applies all the possible combinations (aka our functions above)
;; with an additional opening/closing parenthesis to a given balanced parenthesis list.
;; It does the combinations to all balanced parenthesis lists in the list. 
(define (apply-combo lst accum)
  (cond ((null? lst) accum)
        (else (let* ((curr-lst (car lst))
                     (surround-lst (surround curr-lst))
                     (concatLeft (concat-left curr-lst))
                     (concatRight (concat-right curr-lst))
                     (all-combo (list surround-lst
                                      concatLeft
                                      concatRight))
                     (update-accum (append all-combo accum)))
                (apply-combo (cdr lst) update-accum)))))

;; Recursive function that builds up to the answer
(define (gen-paren length)
  (cond ((= length 2) (list '(0 1)))
        (else (let* ((prev-gen-paren (gen-paren (- length 2)))
                     (new-paren (apply-combo prev-gen-paren '()))
                     (no-dups-paren (remove-dups new-paren)))
                no-dups-paren))))

;; QUESTION 2

;; You are given the following code. Implement let*

#;(define-datatype expression expression?
  (lit-exp (datum number?)) 
  (var-exp (id symbol?)) 
  (primapp-exp (prim primitive?) 
               (rands (list-of expression?))))

(define-datatype primitive primitive?
  (mult-prim)
  (add-prim)
  (subtract-prim)
  (incr-prim)
  (decr-prim))

(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim () (+ (car args) (cadr args)))
      (subtract-prim () (- (car args) (cadr args)))
      (mult-prim () (* (car args) (cadr args)))
      (incr-prim () (+ (car args) 1))
      (decr-prim () (- (car args) 1)))))

(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record 
    (syms (list-of symbol?))
    (vals (list-of scheme-value?))
    (env environment?)))

(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record () (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals env)
        (let ((pos (list-find-position sym syms))) 
          (if (number? pos)
              (list-ref vals pos) 
              (apply-env env sym)))))))

(define init-env
  (lambda () (extend-env '(i v x) '(1 5 10) (empty-env))))

(define scheme-value?
  (lambda (v) #t))

(define empty-env
  (lambda () (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define list-find-position
  (lambda (sym los)
    (let loop ((ls los) (index 0)) 
      (cond
        ((null? ls) #f)                    
        ((eqv? (car ls) sym) index)        
        (else (loop (cdr ls) (+ index 1)))) 
      )))

(define eval-rands
  (lambda (rands env) 
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

#;(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum) 
      (var-exp (id) (apply-env env id)) 
                                       
      (primapp-exp (prim rands)         
        (let ((args (eval-rands rands env)))
          (apply-primitive prim args)))
      )))

;; MY ANSWER:

;; Update expression for a let* expression
(define-datatype expression expression?
  (lit-exp (datum number?)) 
  (var-exp (id symbol?)) 
  (primapp-exp (prim primitive?) 
               (rands (list-of expression?)))
  (let*-exp (ids (list-of symbol?))
            (rands (list-of expression?))
            (body expression?)))

;; Update eval-expression to add let*-exp
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (lit-exp (datum) datum) 
      (var-exp (id) (apply-env env id)) 
                                       
      (primapp-exp (prim rands)         
        (let ((args (eval-rands rands env)))
          (apply-primitive prim args)))
      (let*-exp (ids rands body) (let* ((updated-env (eval-let* ids rands env)))
                                   (eval-expression body updated-env)))
      )))

;; Function to evaluate let*-exp
(define (eval-let* ids rands env)
  (cond ((or (null? ids) (null? rands)) env)
        (else (let* ((curr-id (car ids))
                     (evaluated-exp (eval-expression (car rands) env))
                     (updated-env (extend-env (list curr-id) (list evaluated-exp) env)))
                (eval-let* (cdr ids) (cdr rands) updated-env)))))