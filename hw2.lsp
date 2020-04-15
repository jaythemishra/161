; These functions implement a depth-first solver for the missionary-cannibal
; problem. In this problem, three missionaries and three cannibals are trying to
; go from the east side of a river to the west side. They have a single boat
; that can carry two people at a time from one side of the river to the
; other. There must be at least one person in the boat to cross the river. There
; can never be more cannibals on one side of the river than missionaries. If
; there are, the cannibals eat the missionaries.

; In this implementation, a state is represented by a single list
; (missionaries cannibals side). side represents which side the boat is
; currently on, and is T if it is on the east side and NIL if on the west
; side. missionaries and cannibals represent the number of missionaries and
; cannibals on the same side as the boat. Thus, the initial state for this
; problem is (3 3 T) (three missionaries, three cannibals, and the boat are all
; on the east side of the river) and the goal state is (3 3 NIL).

; The main entry point for this solver is the function MC-DFS, which is called
; with the initial state to search from and the path to this state. It returns
; the complete path from the initial state to the goal state: this path is a
; list of intermediate problem states. The first element of the path is the
; initial state and the last element is the goal state. Each intermediate state
; is the state that results from applying the appropriate operator to the
; preceding state. If there is no solution, MC-DFS returns NIL.

; To call MC-DFS to solve the original problem, one would call (MC-DFS '(3 3 T)
; NIL) -- however, it would be possible to call MC-DFS with a different initial
; state or with an initial path.

; Examples of calls to some of the helper functions can be found after the code.



; FINAL-STATE takes a single argument s, the current state, and returns T if it
; is the goal state (3 3 NIL) and NIL otherwise.
(defun final-state (s)
  (equal s '(3 3 NIL)))

; NEXT-STATE returns the state that results from applying an operator to the
; current state. It takes three arguments: the current state (s), a number of
; missionaries to move (m), and a number of cannibals to move (c). It returns a
; list containing the state that results from moving that number of missionaries
; and cannibals from the current side of the river to the other side of the
; river. If applying this operator results in an invalid state (because there
; are more cannibals than missionaries on either side of the river, or because
; it would move more missionaries or cannibals than are on this side of the
; river) it returns NIL.
;
; NOTE that next-state returns a list containing the successor state (which is
; itself a list); the return should look something like ((1 1 T)).
(defun next-state (s m c)
  (let* ((future_num_missionaries_on_current_boat_side (- (first s) m))
        (future_num_cannibals_on_current_boat_side (- (second s) c))
        (future_num_missionaries_on_current_other_side (- 3 future_num_missionaries_on_current_boat_side))
        (future_num_cannibals_on_current_other_side (- 3 future_num_cannibals_on_current_boat_side)))
    (cond ((or  (< future_num_missionaries_on_current_boat_side 0)
                (< future_num_cannibals_on_current_boat_side 0)
                (and  (> future_num_missionaries_on_current_boat_side 0)
                      (< future_num_missionaries_on_current_boat_side future_num_cannibals_on_current_boat_side))
                (and  (> future_num_missionaries_on_current_other_side 0)
                      (< future_num_missionaries_on_current_other_side future_num_cannibals_on_current_other_side)))
              ;; (print "INVALID") (terpri)
              NIL)
          (t
            ;; (print "VALID") (terpri)
            (list (list future_num_missionaries_on_current_other_side
                        future_num_cannibals_on_current_other_side
                        (not (third s))))))))

; SUCC-FN returns all of the possible legal successor states to the current
; state. It takes a single argument (s), which encodes the current state, and
; returns a list of each state that can be reached by applying legal operators
; to the current state.
(defun succ-fn (s)
  (append (next-state s 0 1)
          (next-state s 0 2)
          (next-state s 1 0)
          (next-state s 1 1)
          (next-state s 2 0)
  ))

; ON-PATH checks whether the current state is on the stack of states visited by
; this depth-first search. It takes two arguments: the current state (s) and the
; stack of states visited by MC-DFS (states). It returns T if s is a member of
; states and NIL otherwise.
(defun on-path (s states)
  (cond ((null states) NIL)
        ((equal s (first states)) t)
        (t (on-path s (rest states)))))

; MULT-DFS is a helper function for MC-DFS. It takes two arguments: a stack of
; states from the initial state to the current state (path), and the legal
; successor states from the current state (states).
; MULT-DFS does a depth-first search on each element of states in
; turn. If any of those searches reaches the final state, MULT-DFS returns the
; complete path from the initial state to the goal state. Otherwise, it returns
; NIL. 
; Note that the path should be ordered as: (S_n ... S_2 S_1 S_0)
(defun mult-dfs (states path)
  (cond ((null states) NIL)
        (t (or  (mc-dfs (first states) path)
                (mult-dfs (rest states) path)))))

; MC-DFS does a depth first search from a given state to the goal state. It
; takes two arguments: a state (S) and the path from the initial state to S
; (PATH). If S is the initial state in our search, PATH should be NIL. MC-DFS
; performs a depth-first search starting at the given state. It returns the path
; from the initial state to the goal state, if any, or NIL otherwise. MC-DFS is
; responsible for checking if S is already the goal state, as well as for
; ensuring that the depth-first search does not revisit a node already on the
; search path.
(defun mc-dfs (s path)
  (cond ((final-state s) (cons s path))
        ((and (equal s '(3 3 T))
              (not (null path)))
            NIL)
        ((on-path s path) NIL)
        (t (mult-dfs (succ-fn s) (cons s path)))))



; Function execution examples

; Applying this operator would result in an invalid state, with more cannibals
; than missionaries on the east side of the river.
; (next-state '(3 3 t) 1 0) -> NIL

; Applying this operator would result in one cannibal and zero missionaries on
; the west side of the river, which is a legal operator. (NOTE that next-state
; returns a LIST of successor states, even when there is only one successor)
; (next-state '(3 3 t) 0 1) -> ((0 1 NIL))

; succ-fn returns all of the legal states that can result from applying
; operators to the current state.
; (succ-fn '(3 3 t)) -> ((0 1 NIL) (1 1 NIL) (0 2 NIL))
; (succ-fn '(1 1 t)) -> ((3 2 NIL) (3 3 NIL))

;; Returns a list containing the leaves of a left to right breadth first search
;; of the tree structure tree, which can be either a list of nodes or an atom,
;; and is the only argument supplied to bfs.
(defun bfs (tree)
  (cond ((atom tree) (list tree))
        ((and (atom (first tree))
              (null (rest tree)))
            tree)
        ((atom (first tree)) (cons  (first tree) 
                                    (bfs (rest tree))))
        (t (bfs (append (rest tree)
                        (first tree))))))

;; Returns a list containing the leaves of a left to right depth first search
;; of the tree structure tree, which can be either a list of nodes or an atom,
;; and is the only argument supplied to bfs.
(defun dfs (tree)
  (cond ((atom tree) (list tree))
        ((= 1 (length tree)) (dfs (first tree)))
        (t (append  (dfs (rest tree))
                    (dfs (first tree))))))

;; Performs the depth first search part of the dfid algorithm and returns a list
;; of the nodes visited in the order of a depth first search traversal.
;; It takes the same arguments as the top level dfid function.
(defun dfid_search (tree depth)
  (cond ((or (null tree) (< depth 0)) NIL)
        ((atom tree) (list tree))
        (t (append  (dfid_search (first tree) (- depth 1))
                    (dfid_search (rest tree) depth)))))                                           

;; The top level function of the implementation of dfid. It takes two arguments, max_depth,
;; which represents the maximum depth to which the algorithm should search, and tree,
;; which represents a tree structure that is being searched and can be a list of nodes
;; or an atom. This function calls dfid_search to do the actual depth first search.
(defun dfid (tree max_depth)
  (cond ((< max_depth 1) NIL)
        (t (append  (dfid tree (- max_depth 1))
                    (dfid_search tree max_depth)))))