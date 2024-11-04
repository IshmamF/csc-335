#lang eopl

; Exercise 2.1 [ ] Implement the four required operations for bigits. Then use it to calculate the factorial of 10.
; How does the execution time vary as this argument changes? How does the execution time vary as the base
; changes? Explain why.

; Exercise 2.4 [ ] Implement a bintree-to-list procedure for binary trees, so that
; (bintree-to-list (interior-node 'a (leaf-node 3) (leaf-node 4)))
; returns the list (interior-node a (leaf-node 3) (leaf-node 4))

; Exercise 2.5 [ ] Use cases to write max-interior, which takes a binary tree of numbers
; with at least one interior node and returns the symbol associated with an interior node with a maximal leaf sum.
; > (define tree-a (interior-node 'a (leaf-node 2) (leaf-node 3)))
; > (define tree-b (interior-node 'b (leaf-node -1) tree-a))
; > (define tree-c (interior-node 'c tree-b (leaf-node 1)))
; > (max-interior tree-b)a> (max-interior tree-c)c
; The last invocation of max-interior might also have returned a, since both the a and c nodes
; have a leaf sum of 5.