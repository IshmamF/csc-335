#lang eopl
;; Review on Chapters 5, 6 & 7
;; Problems to look at (Chp 5): 2.1 2.4 2.5 2.7 2.8
;; Problems to look at (Chp 6): 2.10, 2.11, 2.24, 2.25
;; Problems to look at (Chp 7): 3.3, 3.4, 3.6 3.7 3.10, 3.11, 3.12, 3.13 3.14 3.17
;; most likely to appear: 3.11, 3.13,  Maybe: 3.17
;; attempting 3.10 Test if forms by extending the interpreter of figure 3.2.
;;
;; 
;;
;;
;;
;;
;;

(define-datatype bintree bintree?
  (leaf-node (datum number?))
  (interior-node
   (key symbol?)
   (left bintree?)
   (right bintree?)))

(bintree? 1)


(define aleaf (lambda () (leaf-node 77)))
(define aTree
  (lambda ()
    (interior-node
     'root
     (leaf-node 12)
     (interior-node
      'right-subtree
      (leaf-node 13)
      (leaf-node 14)))))


(bintree? aleaf)
(display (bintree? (aleaf)))
(newline)
(display (bintree? 77))
(newline)
(display (bintree? (aTree)))

(define leaf-sum
  (lambda (tree)
    (cases bintree tree
      (leaf-node (numba) numba)
      (interior-node
       (id left-sub right-sub)
       (display id)
       (newline)
       (+ (leaf-sum left-sub)
          (leaf-sum right-sub))))))

(newline)
(display (leaf-sum (aleaf)))
(newline)
(display (leaf-sum (aTree)))
(newline)
(display (symbol? '(1 2 3)))



(define-datatype expression expression?
  (varexp
    (id symbol?))
  (lambdaexp
    (id symbol?) ;;this is written the exact same way in the quiz which means that we assumed vars to only ever be one parameter in the body.....
    (body expression?))
  (appexp
    (rator expression?)
    (rand expression?)))


(newline)
(define anExp
  (lambda ()
    (appexp (varexp 'k) (lambdaexp 'j (appexp (varexp 'i) (varexp 'j))))))
(display (expression? (anExp)))


(newline)
;;;;;;; this is more like a blend above

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Down here we will take a look at chapter 7 and just do some problems from it since this is the more
;; likely one of the problems will most likely end up on the exam.....
;;
;;
;; <expression> :== <expression> -> (a-program (exp))
;;              :== (number) -> (datum )
;;              :== if <expression> then <expression> else <expression>
;;              :== let {<identifier> = <expression>} then <expression> else <expression>
;;              :== <identifier>
;;              :== (lambda (<identifier>) <expression>)
;;              :== <primitive> ({<expression>}) -> {} means the expression can be repeated 0 or more times
;;
;;
;; The overarching theme of Chpt 3 is
;; essentially building up from a simple interpreter to a more complex one
;; at first we start with a simple one that can take literals
;; primitves and them enacting on literals, and variables holding data/literals
;; In 3.1 we get a simple interpreter
;; that just takes in numbers variables and primitve operations
;; the interpreter also executes these primitive operations and returns it
;;
;;
;;
;;


;;the below is our datatype definitions for 

(define-datatype program program?
  (a-program (exp expression3?)))

(define-datatype expression3 expression3?
  (lit-exp (datum number?)) ;; a literal in our case only numbers
  (bool-exp (bool boolean?))
  (var-exp (id symbol?)) ;; node representing a variable so like x = 3
  (primapp-exp (prim primitive?) 
               (rands (list-of expression3?))) ;; primitives like + - *
  (if-exp (test-exp expression3?)
          (true-exp expression3?)
          (false-exp expression3?))
  (let-exp (ids (list-of symbol?))
           (rands (list-of expression3?))
           (body expression3?))
  (let*-exp (ids (list-of symbol?))
            (rands (list-of expression3?))
            (body expression3?))
  (cond-exp (test (list-of expression3?))
            (rands (list-of expression3?))))

(define-datatype primitive primitive?
  (add-prim)
  (subtract-prim)
  (mult-prim)
  (incr-prim)
  (decr-prim)
  (equal?-prim))


(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record ;; envs -> (syms vals (env))
    (syms (list-of symbol?))
    (vals (list-of scheme-value?))
    (env environment?)))

(define apply-env
  (lambda (env sym)
    (cases environment env
      (empty-env-record () (eopl:error 'apply-env "No binding for ~s" sym))
      (extended-env-record (syms vals env)
        (let ((pos (list-find-position sym syms))) ;;list-find-position finds the index of sym in syms and binds that position number to pos
          (if (number? pos)
              (list-ref vals pos) ;; return the item at position
              (apply-env env sym))))))) ;; otherwise keep searching

(define init-env
  (lambda () (extend-env '(i v x) '(1 5 10) (empty-env))))

(define scheme-value?
  (lambda (v) #t))

(define empty-env
  (lambda () (empty-env-record)))

(define extend-env
  (lambda (syms vals env)
    (extended-env-record syms vals env)))

(define true-value? ;; returns true if incoming number is not 0
  (lambda (x) x))

(define list-find-position
  (lambda (sym los)
    (let loop ((ls los) (index 0)) ; Use a named `let` for recursion with an index counter.
      (cond
        ((null? ls) #f)                     ; Base case: end of the list, return #f.
        ((eqv? (car ls) sym) index)         ; Found the element, return the current index.
        (else (loop (cdr ls) (+ index 1)))) ; Recursive case: move to the next element.
      )))


;;interpreter below....
;;simple interpreter using the above defined datatypes
(define eval-program ;; here we are just ensuring that what was passed is a-program and if so we pass it to evaluate the expression
  (lambda (pgm)
    (cases program pgm
      (a-program (body)
        (eval-expression body (init-env))))))

(define (eval-let* ids rands env)
  (cond ((or (null? ids) (null? rands)) env)
        (else (let* ((curr-id (car ids))
                     (evaluated-exp (eval-expression (car rands) env))
                     (updated-env (extend-env (list curr-id) (list evaluated-exp) env)))
                (eval-let* (cdr ids) (cdr rands) updated-env)))))

;(add-prim (1 2))
(define eval-expression ;;evaluating the entirety of the expression passed
  (lambda (exp env)
    (cases expression3 exp
      (bool-exp (bool) bool)
      (lit-exp (datum) datum) ;;if the exp is a literal just return that data
      (var-exp (id) (apply-env env id)) ;; if exp is a node that represents a variable, we look up the identifier (params)
                                        ;; in the env to find its values
      (primapp-exp (prim rands)         
        (let ((args (eval-rands rands env)))
          (apply-primitive prim args)))
      (if-exp (test-exp true-exp false-exp)
       (if (true-value? (eval-expression test-exp env)) ;;check whether the number passed is 0
           (eval-expression true-exp env) ;; throw into eval expression since we might still need to decode the true-exp & the false-exp
           (eval-expression false-exp env)))
      (let-exp (ids rands body)
               (let ((args (eval-rands rands env)))
                 (eval-expression body (extend-env ids args env))))
      (let*-exp (ids rands body)
                (let* ((update-env (eval-let* ids rands)))
                  (eval-expression body update-env)))
      )))

(cond (((test-expression) (evaluated expression))
       ((test-expression) (evaluated expression))
       ((test-expression) (evaluated expression)))

; Question: Given a length , create all possible balanced parenthesis combinations of size length
; 0 represents opening parenthesis, 1 represents closing parenthesis. There should be no duplicates
; Example: length 2 would give us ((0 1))
; Example: length 4 would give us ((0 0 1 1) (0 1 0 1))

; ( 0 1 ) or ( 0 0 )
; ( 0 1 ) <- 
; ( 0 1 1 ) or ( 0 1 0 )
; ( 0 1 1 ) <- invalid , so im assuming we have to add a case to check if close > open
; we have a set number of open, and a set number of closing <- 2 open and 2 close

; ( 0 1 ) -> how do we get to length 4?
; ( 0 1 0 1 )
; ( 0 0 1 1 )

; 


(define eval-rands
  (lambda (rands env) ;; takes list of operands and an env
    (map (lambda (x) (eval-rand x env)) rands)))
     ;;apply eval-rand to each operand in rands
     ;; essentially to decoded any var-exp that may be in there
     ;; or we just return the literal (in our case number)
     ;; and since its a map rands will be decoded and we will have
     ;; a list of numbers.
     ;; this then gets passed into apply-primitive to process
     ;; of course after passing back the mapped list.
(define eval-rand
  (lambda (rand env)
    (eval-expression rand env)))

(define apply-primitive
  (lambda (prim args)
    (cases primitive prim
      (add-prim ()
        (+ (car args) (cadr args)))
      (subtract-prim ()
        (- (car args) (cadr args)))
      (mult-prim ()
        (* (car args) (cadr args)))
      (incr-prim ()
        (+ (car args) 1))
      (decr-prim ()
        (- (car args) 1))
      (equal?-prim () (if (equal? (car args) (cadr args)) #t #f)))))



;; bunch of displaying so I understand what's going on in this big ass prog.
(display (list-find-position 4 '(1 2 3 4 5 6 8 b)))
(newline)
(display (expression3? (lit-exp 1)))
(newline)
(display (expression3? (var-exp 'a)))
(newline)
(display (list? (list (lit-exp 1) (lit-exp 1)) ))
(newline)

(extend-env '(a b) (list 1 2) (empty-env))
(display (environment? (extended-env-record '(a b) (list 1 2) (empty-env))))
(newline)
(display (primapp-exp (add-prim) (list (lit-exp 1) (lit-exp 2))))
(newline)


;; Doing 3.10: Test if forms by extending the interpreter of figure 3.2.
;; we have already extended the eval-expression to include the if-exp case
;; and we also have added the if-exp datatype definition
(display
 (eval-program
  (a-program
   (if-exp (primapp-exp (equal?-prim) (list (bool-exp #t) (bool-exp #f)))
           (lit-exp 1)
           (lit-exp 0)))))



;;first thing tomorrow try out 
;; these constructors for our primitives
;; are empty because we can basically do stuff like
;; (cases primitive prim-exp (add-prim () (+ (car prim-exp) (cadr prim-exp))))
;;
;;
;; Consider the fourth example above. Then implement the procedure program-tolist so that it returns the list
;;(a-program
;;  (primapp-exp
;;    (incr-prim)
;;    ((primapp-exp
;;       (add-prim)
;;       ((lit-exp 3)
;;        (var-exp x))))))
;;
;; symbol sym, first uses the auxiliary procedure list-find-position to determine the
;; position of sym in syms. The procedure list-find-position, in turn, uses list-index
;; to accomplish this. If sym is in syms, then list-index returns an integer representing its
;; position, and the corresponding element of vals is returned using the procedure list-ref. If
;; sym is not in syms, then list-index returns #f, and sym is looked up in the old environment
;; env, in accordance with the specification.
;;
;;
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Understanding how to do strutural induction PF
;; keep in mind for recursion
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;
;;



