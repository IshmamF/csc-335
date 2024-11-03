; make-la to convert v, d, p into (v : d p)
; check the first element of body , if it's lambda then we increment
; how would we do this without iteration?
;

(define (lexical-address expr)
  (lexical-address-helper expr '() 0))

;; Recursive helper function
(define (lexical-address-helper expr env depth)
  (cond
    ;; Case 1: Identifier (variable)
    ((symbol? expr)
     (find-variable-recursive expr env depth 0))

    ;; Case 2: Lambda expression
    ((and (pair? expr) (eq? (car expr) 'lambda))
     (let ((params (cadr expr))
           (body (caddr expr)))
       ;; Create a new scope by extending the environment with parameters
       (let ((new-env (cons params env)))
         (list 'lambda params
               (lexical-address-helper body new-env (+ depth 1))))))

    ;; Case 3: If expression
    ((and (pair? expr) (eq? (car expr) 'if))
     (let ((test (cadr expr))
           (conseq (caddr expr))
           (alt (cadddr expr)))
       ;; Recursively handle each part of the 'if' expression
       (list 'if
             (lexical-address-helper test env depth)
             (lexical-address-helper conseq env depth)
             (lexical-address-helper alt env depth))))

    ;; Case 4: Application (procedure call)
    (else
     (map (lambda (subexpr) (lexical-address-helper subexpr env depth)) expr))))

;; Recursively find the variable in the environment without explicit iteration
(define (find-variable-recursive var env depth current-depth)
  (cond
    ;; If environment is empty, the variable is free
    ((null? env) (list var 'free))

    ;; If variable is found in the first scope, return its position
    ((let ((scope (car env)))
       (if (member var scope)
           (list var current-depth (get-position var scope 0))
           ;; If not found in this scope, go deeper
           (find-variable-recursive var (cdr env) depth (+ current-depth 1)))))))

;; Helper to get position of a variable in the current scope without iteration
(define (get-position var scope position)
  (cond
    ((null? scope) #f) ;; should never happen if var exists in scope
    ((eq? var (car scope)) position)
    (else (get-position var (cdr scope) (+ position 1)))))
