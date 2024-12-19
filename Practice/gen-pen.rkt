#lang racket

(define (surround lst)
  (let* ((add-0 (cons 0 lst))
         (reversed (reverse add-0))
         (add-1 (cons 1 reversed)))
    (reverse add-1)))

(define (concat-left lst)
  (append '(0 1) lst))

(define (concat-right lst)
  (let* ((reversed (reverse lst))
         (add-10 (append '(1 0) reversed)))
    (reverse add-10)))

(define (remove-dups lst)
  (cond ((null? lst) '())
        ((member (car lst) (cdr lst)) (remove-dups (cdr lst)))
        (else (cons (car lst) (remove-dups (cdr lst))))))

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

(define (gen-paren length)
  (cond ((= length 2) (list '(0 1)))
        (else (let* ((prev-gen-paren (gen-paren (- length 2)))
                     (new-paren (apply-combo prev-gen-paren '()))
                     (no-dups-paren (remove-dups new-paren)))
                no-dups-paren))))
