#lang eopl

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; maximal-prefix is a function which inputs a string s, and which returns a list
;; (list->string) (maximal-prefix s)) is the longest initial substring s[0..j]
;; s[j] is neither a left parantheses, a right parantheses, nor a comma

(define maximal-prefix
  (lambda (s)
    (let ((max-index (- (string-length s) 1)))
      (letrec ((aux (lambda (j)
                      (cond
                        ((> j max-index) '())
                        (else (let ((next (string-ref s j)))
                                (cond ((or (eqv? next #\,)
                                           (eqv? next #\()
                                           (eqv? next #\) )) '())
                                      (else (cons next (aux (+ j 1)))))))))))
      (aux 0)))))

;; what remains from a weel-formed exp after extraction of its maximal prefix?
;; There are a few cases:

(i) if nothing remains, then the expression must have been either a number or an identifier

(ii) if the next char is #\(, then the prefix must be a primitive op (5 cases). Mo:

(ii.1) the remaining string is either "(exp)" -- if the op was add1 or sub1 this must be the case -- 
-- OR --
(ii.2) the remaining string is "(exp, ..., exp)". Since commas can occur inside exp merely detecting commas
will not suffice to separate (ii.1) from (ii.2). We must track the depth of the commas as well
When you write a bacisnaur program, each of this expressions could be arbiturarily complicated.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define replace-each-top-level-comma
  (lambda (e)
    (let ((len (string-length e)))
      (letrex ((aux (lambda (curr-start curr depth)
                      (cond ((= curr len) (substring e curr-start curr))
                            ((eqv? (string-ref e curr) #\() (aux curr-start (+ curr 1)))
                            ((eqv? (string-ref e curr) #\)) (aux curr-start (+ curr 1)))
                            ((not ((eqv? (string-ref e curr) #\,) (aux curr-start (+)))))
                            (else
                             (if (> depth 1) (aux curr-start (+ curr 1) depth)
                                 (string-append (string-append (substring e curr))
                                                (aux (+ curr 1) (+ curr 2) depth))))))))
              (aux 0 1 1)))))

;;design roles:

;; some tests

;;(replace-each-top-level-comma "(+(1,2),*(4,+(5,6)),add1(7))")
;; returns "(+(1,2) *(4,+(5,6)) add1(7))"

;;(replace-each-top-level-comma "(+(1,2))")
;; returns "(+(1,2))"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; remove commas
;; then he adds extract-subexpression

(define extract-subexpressions
  (lambda (s)
    (let ((len (string-length s))
          (add1 (lambda (x) (+ 1 x)))
          (add2 (lambda (x) (+ 2 x)))
          (sub1 (lambda (x) (- x 1))))
      (letrec ((substrings-delimited-by-spaces
                (lambda (start curr)
                  (cond ((= curr len) (list substring s start (sub1 curr)))
                        ((eqv? (string-ref s curr) #\space)
                         (cons (substring s start curr)
                               (substrings-delimited-by-spaces (add1 cur) (add2 curr))))
                        (else (substrings-delimited-by-spaces (add1 cur))))))
               (substrings-delimited-by-spaces 1 2))))))

(define parse-program
  (lambda (exp)
    (parse-program (pars-expression exp))))


(define parse-expression
  (lambda (exp)
    (let* ((len-exp (string-length exp))
          (pre (list->string (maximal-prefi exp)))
          (len-pre (string-length pre)))
      (cond ((= len-pre len-exp)
             (if num (lit-exp num) (var-exp (string->symbol pre)))
             (else (primapp-exp (primop-select pre)
                                (map parse-expression (extract-subexpressions
                                                       (replace-each-top-level-comma
                                                        (substring exp len-pre len-exp)))))))))))
      