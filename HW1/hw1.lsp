; function PAD takes in an input N 
; returns Nth Padovan number
(defun PAD (N) 
	(cond 	((= N 0) 1) ; base case 
		((= N 1) 1) ; base case
		((= N 2) 1) ; base case
		(t (+ (PAD (- N 2)) (PAD (- N 3)))))) ; recursive case
					  	

; function SUMS takes in an input N
; Returns the number of additions required to compute (PAD N)
(defun SUMS (N)
	(cond 	((= N 0) 0) ; base case
		((= N 1) 0) ; base case
		((= N 2) 0) ; base case
		(t (+ (SUMS (- N 2)) (SUMS (- N 3)) 1)))) ; recursive case


; function ANON takes in an input TREE
; Replaces all the data with '?'
(defun ANON (TREE)
	(cond	((null TREE) '()) ; return empty list of the tree is empty 
		((atom TREE) '?) ; return a single ? if there is only one atom
		(t (cons (ANON (car TREE)) (ANON (cdr TREE)))))) ; recursive case


