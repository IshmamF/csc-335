#lang eopl

;; Section 3.1

;; program datatype

(define-datatype program program?
  (a-program (exp expression?)))

(define-datatype expression expression?
  (lit-exp (datum number?))
  (var-exp (id symbol?))
  (primapp-exp (prim primitive?)
               (rands (list-of expression?))))
  

(define-datatype primitive primitive?
  (add-prim)
  (subtract-prim)
  (mult-prim)
  (incr-prim)
  (decr-prim))


;; A program is just an expression.

;; An expression is either a number, an identifier, or a primitive application
;; consisting of a primitive operator, a left parenthesis, a list of expressions
;; separated by commas, and a right parenthesis.

;;      exp ::=  number | identifier |  primop(exp,...,exp)

;; Typical expressions in our language are

;; 3

;; x

;; +(3,x)

;; add1(+(3,x))



;; We want program-to-list, as specified in the text.

;; It seems we must start by regarding these expressions as strings.  The first problem is to
;; extract tokens -- eg, a maximal substring "123" must be understood as the number 123,
;; and a maximal substring "ident46" -- no letters or numbers in the next position --
;; must be understood as the symbol ident46

;; Following the text's examples: no whitespace characters may occur in these expression strings


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; maximal-prefix is a function which inputs a string s, and which returns a list such that
;; (list->string (maximal-prefix s)) is the longest initial substring s[0..j] such that
;; s[j] is neither  a left parentheses, a right parentheses, nor a comma

(define maximal-prefix
  (lambda (s)
    (let ((max-index (- (string-length s) 1)))
      (letrec ((aux (lambda (j)
                      (cond
                        ((> j max-index) '())
                        (else (let ((next (string-ref s j)))
                                (cond ((or (eqv? next #\,)
                                           (eqv? next #\( )
                                           (eqv? next #\) )) '() )
                                      (else (cons next (aux (+ j 1)))))))))))
        (aux 0)))))




;; what remains from a well-formed exp after extraction of its maximal prefix?  There are
;; a few cases:

;; (i) if nothing remains, then the expression must have been either a number or an identifier

;; (ii) if the next char is #\(, then the prefix must be a primitive op (5 cases).  More,

;; (ii.1) the remaining string is either "( exp )" -- if the op was add1 or sub1 this
;;        must be the case -- or

;; (ii.2) the remaining string is "(exp, ..., exp)".  Since commas can occur inside exps,
;;        merely detecting commas will not suffice to separate (ii.1) from (ii.2).
;;        We must track the depth of the commas as well. 



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; say that a comma at parentheses nesting depth 1 is a top-level comma

;; replace-each-top-level-comma-by-space inputs a substring s of an expression string e and outputs
;; s' obtained from s by replacing each top level comma by a space.  Here s is assumed to be of the
;; form (exp1,...,expk), and the top-level commas are the ones remaining when each of exp1, ..., expk
;; is replaced by xk -- ie, by a simple subscripted variable, with no internal commas. 

;; this is useful in extracting top-level exp substrings, since whitespace does not occur in any well-formed exp

(define replace-each-top-level-comma
  (lambda (e)
    (let ((len (string-length e)))
      (letrec ((aux (lambda (curr-start curr depth)
                      (cond ((= curr len) (substring e curr-start curr))
                            ((eqv? (string-ref e curr) #\() (aux curr-start (+ curr 1) (+ depth 1)))
                            ((eqv? (string-ref e curr) #\)) (aux curr-start (+ curr 1) (- depth 1)))
                            ((not (eqv? (string-ref e curr) #\,)) (aux curr-start (+ curr 1) depth))
                            (else
                             (if (> depth 1) (aux curr-start (+ curr 1) depth)
                                 (string-append (string-append (substring e curr-start curr) " ")
                                                (aux (+ curr 1) (+ curr 2) depth))))))))
               (aux 0 1 1)))))
                            
        
;; design roles:  curr is the string index of the first unprocessed string element
;; depth is the parentheses nesting depth of e[curr]
;; curr-start is the index of the first element of e which has not yet been included in the output

;; from the r5rs manual: (substring string start end) returns a newly allocated string consisting of
;; characters from the input string with first index start, and last index (- end 1).

;; some tests

;; (replace-each-top-level-comma "(+(1,2),*(4,+(5,6)),add1(7))")
;;    returns    "(+(1,2) *(4,+(5,6)) add1(7))"

;; (replace-each-top-level-comma "(+(1,2))")
;;    returns    "(+(1,2))"


                         
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; assumes s is a string of length len >= 3, with s[0] = #\( and s[len - 1] = #\), and which has the
;; appearance of a list of expressions separated by spaces.  Again, whitespace cannot occur within
;; an expression. 

(define extract-subexpressions
  (lambda (s)
    (let ((len (string-length s))
          (add1 (lambda (x) (+ 1 x)))
          (add2 (lambda (x) (+ 2 x)))
          (sub1 (lambda (x) (- x 1))))
      (letrec ((substrings-delimited-by-spaces
                (lambda (start curr)
                  (cond ((= curr len) (list (substring s start (sub1 curr))))
                        ((eqv? (string-ref s curr) #\space)
                         (cons (substring s start curr)
                               (substrings-delimited-by-spaces (add1 curr) (add2 curr))))
                        (else (substrings-delimited-by-spaces start (add1 curr)))))))
        (substrings-delimited-by-spaces 1 2)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define primop-select
  (lambda (s)
    (cond ((equal? s "+") (add-prim))
          ((equal? s "-") (subtract-prim))
          ((equal? s "*") (mult-prim))
          ((equal? s "add1") (incr-prim))
          ((equal? s "sub1") (decr-prim)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(define parse-program
  (lambda (exp)
    (a-program (parse-expression exp))))


(define parse-expression
  (lambda (exp)
    (let* ((len-exp (string-length exp))
           (pre (list->string (maximal-prefix exp)))
           (len-pre (string-length pre))
           (num (string->number pre)))
      (cond ((= len-pre len-exp)
             (if num (lit-exp num) (var-exp (string->symbol pre))))
            (else (primapp-exp (primop-select pre)
                               (map parse-expression (extract-subexpressions
                                                      (replace-each-top-level-comma
                                                       (substring exp len-pre len-exp))))))))))
      



;; some tests


 ;; some tests

 


;; (parse-program "3")

;; (parse-program "x")

;; (parse-program "+(3,x)")

;;(parse-program "add1(+(3,x))")



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; and, finally, the code requested by this (just a single * ??? - am I missing something?) exercise, program-to-list

(define expression-to-list
  (lambda (exp)
    (cases expression exp
      (lit-exp (datum) (list 'lit-exp datum))
      (var-exp (id) (list 'var-exp id))
      (primapp-exp (prim rands)
                   (list 'primapp-exp
                         (cases primitive prim
                           (add-prim () (list 'add-prim))
                           (subtract-prim () (list 'subtract-prim))
                           (mult-prim () (list 'mult-prim))
                           (incr-prim () (list 'incr-prim))
                           (decr-prim () (list 'decr-prim)))
            (map expression-to-list rands))))))
                   
(define program-to-list
  (lambda (prgm)
    (cases program prgm
      (a-program (exp)
                 (list 'a-program (expression-to-list exp))))))


;; some tests

;; (program-to-list (parse-program "add1(+(3,x))"))

