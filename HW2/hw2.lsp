;;;;;;;;;;;;;;
; Homework 2 ;
;;;;;;;;;;;;;;

;;;;;;;;;;;;;;
; Question 1 ;
;;;;;;;;;;;;;;

; function BFS takes in argument FRINGE which is a list of search trees
; and returns a list of leaf nodes in the order 
; they are visited by left-to-right breadth-first search 
(defun BFS (FRINGE) 
  (cond ((null FRINGE) NIL) ; return nil if the node is null
	((listp (car FRINGE)) (BFS (append (cdr FRINGE) (car FRINGE)))) ; recursive call on appending the cdr to the car
	(T (cons (car FRINGE) (BFS (cdr FRINGE)))))) ; append car to the BFS of the cdr

;;;;;;;;;;;;;;
; Question 2 ;
;;;;;;;;;;;;;;


; These functions implement a depth-first solver for the homer-baby-dog-poison
; problem. In this implementation, a state is represented by a single list
; (homer baby dog poison), where each variable is T if the respective entity is
; on the west side of the river, and NIL if it is on the east side.
; Thus, the initial state for this problem is (NIL NIL NIL NIL) (everybody 
; is on the east side) and the goal state is (T T T T).

; The main entry point for this solver is the function DFS, which is called
; with (a) the state to search from and (b) the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, DFS returns NIL.
; To call DFS to solve the original problem, one would call 
; (DFS '(NIL NIL NIL NIL) NIL) 
; However, it should be possible to call DFS with a different initial
; state or with an initial path.

; First, we define the helper functions of DFS.

; FINAL-STATE takes a single argument S, the current state, and returns T if it
; is the goal state (T T T T) and NIL otherwise.
(defun FINAL-STATE (S)
    (cond
	((equal S '(T T T T)) T) ; goal state
	(T NIL))) ; nil otherwise

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (S), and which entity
; to move (A, equal to h for homer only, b for homer with baby, d for homer 
; with dog, and p for homer with poison). 
; It returns a list containing the state that results from that move.
; If applying this operator results in an invalid state (because the dog and baby,
; or poisoin and baby are left unsupervised on one side of the river), or when the
; action is impossible (homer is not on the same side as the entity) it returns NIL.
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((NIL NIL T T)).
(defun NEXT-STATE (S A)
  (cond
        ((equal A 'h) ; move homer
            (cond
                ((or (equal (second S) (third S)) (equal (second S) (third S))) NIL) ; invalid state if the baby is alone with dog or poison
                (T (list (cons (NOT (car S)) (cdr S)))))) ; otherwise valid so move homer 
        ((equal A 'b) ; move homer and baby
            (cond 
                ((equal (car S) (second S)) ; homer and baby are on the same side
                (list (list (NOT (car S)) (NOT (second S)) (third S) (fourth S))))
                (T NIL))) ; invalid
        ((equal A 'd) ; move homer and dog
            (cond ((not (equal (car S) (third S))) NIL) ; homer and dog not on the same side
                  ((equal (second S) (fourth S)) NIL) ; invalid because baby will be left with poison
                  (T (list (list (NOT (car S)) (second S) (NOT (third S)) (fourth S)))))) ;valid 
        ((equal A 'p) ; move homer and poison
            (cond ((not (equal (car S) (fourth S))) NIL) ; homer and poison on different sides
                  ((equal (second S) (third S)) NIL) ; dog and baby will be together so invalid
                  (T (list (list (NOT (car S)) (second S) (third S) (NOT (fourth S))))))) ; valid 
        (T NIL))) ; invalid argument



; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun SUCC-FN (S)
  ;append all the possible next states together to form a list
    (append (NEXT-STATE S 'h) (NEXT-STATE S 'b) (NEXT-STATE S 'd) (NEXT-STATE S 'p)))

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (S) and the
; stack of states visited by DFS (STATES). It returns T if s is a member of
; states and NIL otherwise.

(defun ON-PATH (S STATES)
    (cond 
        ((null STATES) NIL) ; 
        ((equal (car STATES) S) T) ; current state
        (T (ON-PATH S (cdr STATES))))) ; check the remaining states

; MULT-DFS is a helper function for DFS. It takes two arguments: a list of
; states from the initial state to the current state (PATH), and the legal
; successor states to the last, current state in the PATH (STATES). PATH is a
; first-in first-out list of states; that is, the first element is the initial
; state for the current search and the last element is the most recent state
; explored. MULT-DFS does a depth-first search on each element of STATES in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL.

(defun MULT-DFS (STATES PATH)
    (cond
     ((null STATES) NIL) ; all states have been searched
        ((ON-PATH (car STATES) PATH) (MULT-DFS (cdr STATES) PATH)) ; have already looked at this state
        (T (let ((result (DFS (car STATES) PATH))) ; store the current state in result 
                (cond 
                    ((null result) (MULT-DFS (cdr STATES) PATH)) ; no solution found so keep checking
                    (T result)))))) ; found the solution


; DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH is set to NIL. DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun DFS (S PATH)
    (cond
        ((FINAL-STATE S) (append PATH (list S))) ; goal state was found so append it to the path
        (T (MULT-DFS (SUCC-FN S) (append PATH (list S)))))) ; otherwise keep exploring
    
