; Problem: Write the procedure un-lexical-address, which takes lexical-address expressions with formal parameter lists and with variable references of the
; form (: d p), or (v free) and returns an equivalent expression formed by substituting standard
; variable references for the lexical-address information, or #f if no such expression exists.

; Examples:
; > (un-lexical-address '(lambda (a) (lambda (b c) ((: 1 0) (: 0 0) (: 0 1)))))
; (lambda (a) (lambda (b c) (a b c)))
; > (un-lexical-address '(lambda (a) (lambda (a) (: 1 0))))
; #f

; Design Idea:
; We follow the same recursion process as in 4.31 to go through all the possible BNF rules, and we will also have an "env" that will
; hold all the scopes.
; Our base case is that we have a list of three elements where the first element is a colon, and in that case, we'll
; use a helper function that will take in the env, depth, and position, and locate the variable within the env using that information.
; If we don't find a variable at that depth/position, we return #f.
; Which will map (: d p) to (variable d p), we then go through our environment again, and check if the depth and position match. 


; Concerns: Is it possible to locate the wrong variable?

; Data structure :

; Helper Functions:

; pre: a list of s-exp (lst) and a function that handles each element within the list (f)
; post: the same list of lists , but the function applied to each list
(define mymap
  (lambda (f lst)
    (cond ((null? lst) '())
          (else (cons (f (car lst)) (map f (cdr lst)))))))

; pre: takes in a list of lists (env), and two numbers
; post: returns a symbol or #f
(define locate-var
  (lambda (env depth position)
    (cond ((null? env) #f)
          ((= depth 0)(var-at-pos (car env) position))
          ((< depth 0) #f)
          (else (locate-var (cdr env) (- depth 1) position)))))

(define var-at-pos
  (lambda (lst position)
    (cond ((null? lst) #f)
          ((= position 0) (car lst))
          (else (var-at-pos (cdr lst) (- position 1)))
          )))

; pre: expects a symbol (var) and a list of atoms (lst)
; post: returns a number
(define getPos
  (lambda (var lst)
    (cond ((null? lst) #f)
          ((eq? var (car lst)) 0)
          (else (+ 1 (getPos var (cdr lst))))
          )))

; pre: expects a list of lists (env), a symbol (var), and 3 numbers
; post: returns either a boolean value or var
(define check-var
  (lambda (env var curr depth pos)
    (cond ((null? env) #f)
          ((member var (car env))
           (let* ((actualPos (getPos var (car env))))
             (if (AND (= depth curr) (= pos actualPos))
                 var
                 #f
                 )))
          (else (check-var (cdr env) var (+ curr 1) depth pos))
          )))


; pre: list of s-exp
; post: boolean value
(define false-exists
  (lambda (output)
    (if (member '#f (flatten output))
        #t
        #f
        )))


; pre: list of s-exp
; post: a list of all the elements within the list, with elements of sub-lists being individual elements within the main list.
(define flatten
  (lambda (lst)
    (cond ((null? lst) '())
          ((list? (car lst)) (append (flatten (car lst)) (flatten (cdr lst))))
          ((symbol? (car lst)) (append (list (car lst)) (flatten (cdr lst))))
          (else (cons (car lst) (flatten (cdr lst)))))))
               
          

; Constructors

(define make-lambda
  (lambda (a b)
    (list 'lambda a b)))

(define make-if
  (lambda (a b c)
    (list 'if a b c)))

; Selectors

(define first
  (lambda (exp)
    (car exp)))

(define second
  (lambda (exp)
    (cadr exp)))

(define third
  (lambda (exp)
    (caddr exp)))

(define fourth
  (lambda (exp)
    (cadddr exp)))

; Classifiers

(define check-lambda
  (lambda (exp)
    (eq? (first exp) 'lambda)))

(define check-if
  (lambda (exp)
    (eq? (first exp) 'if)))

(define check-form
  (lambda (exp)
    (and (= (length exp) 3) (eq? (first exp) ':))))


; Main Program
; Pre: lexical-address expressions with formal parameter lists and with variable references of the form (: d p), or (v free)
; Post: an equivalent expression formed by substituting standard variable references for the lexical-address information,
; or #f if no such expression exists.

(define un-lexical-address
  (lambda (exp)
    (define helper
      (lambda (exp env)
        (cond ((check-form exp)(let* ((depth (second exp))
                                      (curr 0)
                                      (pos (third exp))
                                      (var (locate-var env depth pos)))
                                 (if (symbol? var)
                                     (check-var env var curr depth pos) ; want to compare actual depth and position
                                     #f)
                                 ))
              ((check-lambda exp)(make-lambda (second exp)
                                              (helper (third exp)
                                                      (cons (second exp) env))))
              ((check-if exp)(make-if (helper (second exp) env)
                                      (helper (third exp) env)
                                      (helper (fourth exp) env)))
              (else (mymap (lambda (sub-exp) (helper sub-exp env)) exp))
              )))
    (let* ((output (helper exp '())))
      (if (false-exists output)
          #f
          output)
      )))

(un-lexical-address '(lambda (a) (lambda (b c) ((: 1 0) (: 0 0) (: 0 1)))))
(un-lexical-address '(lambda (a) (lambda (a) (: 1 0))))
