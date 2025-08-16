This is my framework for structural induction. Regular induction is pretty much the same thing, except there's no structure. For induction proofs, refer to my quiz 1 and part 2 project. 

## Inductive Hypothesis
In IH - talk about simpler / smaller components. We want to mention two things
1. they work
2. what it correctly returns
ex.
```scheme
; IH: We assume that the simpler components of the s-expression works. More specifically, the function
; correctly returns whether or not the simpler components are an l-expression or not as a boolean
; value.
```
## Base Case
Base case - the simplest case, it's usually trivial to prove because it's one value
## Inductive Step
IS - talk about each possible structure / cases <- these are usually conditionals. Like the checks for if-exp, lambda-exp, etc. or BNF structure or the cases for a datatype (hence why it's called structural induction.
For each case you want to do a few things:
1. Mention IH ex. "By the IH" (this is important)
2. mention that the value we'll recursively call on is a simpler component
3. "By the IH, the code where you handle the recursive call is correct because recursive call returns such and such values and the logic im applying at current call with these values make the overall program correct"
ex.
```scheme
; Case 1:(NOT <l-exp>)
; We can see that <l-exp> is a simpler component of the s-expression and we process it in the next call.
; By the IH, (lexp? (second s-exp)) holds
; because we know whether the s-expression is an l-expression or not, and
; simply have to return that boolean value. 

; Case 2: (AND <l-exp1> <l-exp2>) and (OR <l-exp1> <l-exp2>)
; We can see that <l-exp1> <l-exp2> are simpler components of the s-expression and we process both in the next call.
; By the IH,
; (AND (lexp? (second s-exp)) (lexp? (third s-exp))) 
; holds
; because we know whether either of the components are l-expressions or not, and
; simply have to do the AND of both boolean values to check if current s-expression is
; an l-expression.
```
## Termination
termination - you can only have a finite number of simpler components. Mentioning why the program is guaranteed to stop.
