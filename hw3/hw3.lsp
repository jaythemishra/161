;
; CS161 Hw3: Sokoban
; 
; *********************
;    READ THIS FIRST
; ********************* 
;
; All functions that you need to modify are marked with 'EXERCISE' in their header comments.
; Do not modify a-star.lsp.
; This file also contains many helper functions. You may call any of them in your functions.
;
; *Warning*: The provided A* code only supports the maximum cost of 4999 for any node.
; That is f(n)=g(n)+h(n) < 5000. So, be careful when you write your heuristic functions.
; Do not make them return anything too large.
;
; For Allegro Common Lisp users: The free version of Allegro puts a limit on memory.
; So, it may crash on some hard sokoban problems and there is no easy fix (unless you buy 
; Allegro). 
; Of course, other versions of Lisp may also crash if the problem is too hard, but the amount
; of memory available will be relatively more relaxed.
; Improving the quality of the heuristic will mitigate this problem, as it will allow A* to
; solve hard problems with fewer node expansions.
; 
; In either case, this limitation should not significantly affect your grade.
; 
; Remember that most functions are not graded on efficiency (only correctness).
; Efficiency can only influence your heuristic performance in the competition (which will
; affect your score).
;  
;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; General utility functions
; They are not necessary for this homework.
; Use/modify them for your own convenience.
;

;
; For reloading modified code.
; I found this easier than typing (load "filename") every time. 
;
(defun reload()
  (load "hw3.lsp"))

;
; For loading a-star.lsp.
;
(defun load-a-star()
  (load "a-star.lsp"))

;
; Reloads hw3.lsp and a-star.lsp
;
(defun reload-all()
  (reload)
  (load-a-star))

;
; A shortcut function.
; goal-test and next-states stay the same throughout the assignment.
; So, you can just call (sokoban <init-state> #'<heuristic-name>).
; 
;
(defun sokoban (s h)
  (a* s #'goal-test #'next-states h))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; end general utility functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; We now begin actual Sokoban code
;

; Define some global variables
(setq blank 0)
(setq wall 1)
(setq box 2)
(setq keeper 3)
(setq goal 4)
(setq boxgoal 5)
(setq keepergoal 6)

; Some helper functions for checking the content of a square
(defun isBlank (v)
  (= v blank))

(defun isWall (v)
  (= v wall))

(defun isBox (v)
  (= v box))

(defun isKeeper (v)
  (= v keeper))

(defun isGoal (v)
  (= v goal))

(defun isBoxGoal (v)
  (= v boxgoal))

(defun isKeeperGoal (v)
  (= v keepergoal))

;
; Helper function of getKeeperPosition
;
(defun getKeeperColumn (r col)
  (cond ((null r) nil)
	(t (if (or (isKeeper (car r)) (isKeeperGoal (car r)))
	       col
	     (getKeeperColumn (cdr r) (+ col 1))
	     );end if
	   );end t
	);end cond
  )

;
; getKeeperPosition (s firstRow)
; Returns a list indicating the position of the keeper (c r).
; 
; Assumes that the keeper is in row >= firstRow.
; The top row is the zeroth row.
; The first (right) column is the zeroth column.
;
(defun getKeeperPosition (s row)
  (cond ((null s) nil)
	(t (let ((x (getKeeperColumn (car s) 0)))
	     (if x
		 ;keeper is in this row
		 (list x row)
		 ;otherwise move on
		 (getKeeperPosition (cdr s) (+ row 1))
		 );end if
	       );end let
	 );end t
	);end cond
  );end defun

;
; cleanUpList (l)
; returns l with any NIL element removed.
; For example, if l is '(1 2 NIL 3 NIL), returns '(1 2 3).
;
(defun cleanUpList (L)
  (cond ((null L) nil)
	(t (let ((cur (car L))
		 (res (cleanUpList (cdr L)))
		 )
	     (if cur 
		 (cons cur res)
		  res
		 )
	     );end let
	   );end t
	);end cond
  );end 

;; Returns true if the keeper is on a goal in the provided row, nil otherwise.
(defun keeper-on-goal-row (row)
	(cond ((= 0 (length row)) nil)
        ((isKeeper (first row)) nil)
        ((isKeeperGoal (first row)) t)
	      (t (keeper-on-goal-row (rest row)))))

;; Returns true if the keeper is on a goal in the provided state s, nil otherwise.
(defun keeper-on-goal (s)
  (cond ((= 0 (length s)) nil)
        ((keeper-on-goal-row (first s)) t)
        (t (keeper-on-goal (rest s)))))

;; Returns the number of boxes not on a goal in the provided row.
(defun num-boxes-not-on-goal (row)
  (cond ((= 0 (length row)) 0)
        ((isBox (first row)) (+ 1 (num-boxes-not-on-goal (rest row))))
        (t (num-boxes-not-on-goal (rest row)))))

; EXERCISE: Modify this function to return true (t)
; if and only if s is a goal state of the game.
; (neither any boxes nor the keeper is on a non-goal square)
;
; Currently, it always returns NIL. If A* is called with
; this function as the goal testing function, A* will never
; terminate until the whole search space is exhausted.
;
(defun goal-test (s)
  (and (= 0 (h1 s)) (keeper-on-goal s)));end defun

; EXERCISE: Modify this function to return the list of 
; sucessor states of s.
;
; This is the top-level next-states (successor) function.
; Some skeleton code is provided below.
; You may delete them totally, depending on your approach.
; 
; If you want to use it, you will need to set 'result' to be 
; the set of states after moving the keeper in each of the 4 directions.
; A pseudo-code for this is:
; 
; ...
; (result (list (try-move s UP) (try-move s DOWN) (try-move s LEFT) (try-move s RIGHT)))
; ...
; 
; You will need to define the function try-move and decide how to represent UP,DOWN,LEFT,RIGHT.
; Any NIL result returned from try-move can be removed by cleanUpList.
; 
;
(defun next-states (s)
  (let* ((pos (getKeeperPosition s 0))
	      (y (car pos))
        (x (cadr pos))
        ;x and y are now the coordinate of the keeper in s.
        (result (list (try-move s x y "UP") (try-move s x y "DOWN") (try-move s x y "LEFT") (try-move s x y "RIGHT")))
    )
      (cleanUpList result);end
  );end let
);

;; Gets the integer value of the square on the given row and column on the given state s.
;; If either the row or column numbers are invalid, this function returns the value for a wall.
(defun get-square (s r c)
  (cond ((or  (< r 0)
              (< c 0)
              (> r (- (length s) 1))
              (> c (- (length (first s)) 1)))
            wall)
        (t (elt (elt s r) c))))

;; Returns a new state with the original state having new value v at position (r, c)
(defun set-square (s r c v)
  (append (subseq s 0 r)
          (list (let ((row (elt s r)))
            (append (subseq row 0 c)
                    (list v)
                    (subseq row (+ 1 c)))))
          (subseq s (+ 1 r))))

;; Sets the current square that the keeper is on to the correct value when the keeper moves off.
(defun set-current-keeper-square (current_square s x y)
  (cond ((isKeeper current_square) (set-square s x y blank))
        ((isKeeperGoal current_square) (set-square s x y goal))
        (t nil)))

;; Tries to execute a move in a given direction. Returns the new state if the move is valid, nil otherwise.
(defun try-move (s x y dir)
  (let* ( (next_x (cond ((equal dir "UP") (- x 1))
                        ((equal dir "DOWN") (+ x 1))
                        (t x)))
          (next_y (cond ((equal dir "LEFT") (- y 1))
                        ((equal dir "RIGHT") (+ y 1))
                        (t y)))
          (next_next_x (cond  ((equal dir "UP") (- x 2))
                              ((equal dir "DOWN") (+ x 2))
                              (t x)))
          (next_next_y (cond  ((equal dir "LEFT") (- y 2))
                              ((equal dir "RIGHT") (+ y 2))
                              (t y)))
          (square (get-square s next_x next_y))
          (current_square (get-square s x y)))
    (cond ((isWall square) nil)
          ((isBlank square)
            ;; move the keeper to this square
            (let ((next_s (set-square s next_x next_y keeper)))
              (set-current-keeper-square current_square next_s x y)))
          ((isGoal square)
            ;; move the keeper onto the goal
            (let ((next_s (set-square s next_x next_y keepergoal)))
              (set-current-keeper-square current_square next_s x y)))
          ((or (isBox square) (isBoxGoal square))
            ;; do all the complicated box stuff
            (let ((next_square (get-square s next_next_x next_next_y)))
              (cond ((or  (isBlank next_square)
                          (isGoal next_square))
                        (let* ((next_s (set-square s next_next_x next_next_y (cond ((isBlank next_square) box) (t boxgoal))))
                              (next_next_s (set-square next_s next_x next_y (cond ((isBox square) keeper) (t keepergoal)))))
                            (set-current-keeper-square current_square next_next_s x y)))
                    (t nil))))
          (t nil))))

; EXERCISE: Modify this function to compute the trivial 
; admissible heuristic.
;
(defun h0 (s) 0)

; EXERCISE: Modify this function to compute the 
; number of misplaced boxes in s.
; h1 is an admissible heuristic, because the minimum number of moves to reach the solution
; would equal the number of boxes not currently on goals.
(defun h1 (s)
  (cond ((= 0 (length s)) 0)
        (t (+ (num-boxes-not-on-goal (first s)) (h1 (rest s))))))

; EXERCISE: Change the name of this function to h<UID> where
; <UID> is your actual student ID number. Then, modify this 
; function to compute an admissible heuristic value of s. 
; 
; This function will be entered in the competition.
; Objective: make A* solve problems as fast as possible.
; The Lisp 'time' function can be used to measure the 
; running time of a function call.
;
;; (defun h704925466 (s) (h1 s))
(defun h704925466 (s) (+ (h1 s) (box-in-corner s)))

(defun box-in-corner-row-util (s row r c)
  (cond ((= 0 (length row)) nil)
        ((and (isBox (first row))
            (let ((left (get-square s r (- c 1)))
                  (right (get-square s r (+ c 1)))
                  (up (get-square s (- r 1) c))
                  (down (get-square s (+ r 1) c))
                  (left_up (get-square s (- r 1) (- c 1)))
                  (right_up (get-square s (- r 1) (+ c 1)))
                  (left_down (get-square s (+ r 1) (- c 1)))
                  (right_down (get-square s (+ r 1) (+ c 1))))
                (or (and (isWall left) (or (isWall up) (isWall down)))
                    (and (isWall right) (or (isWall up) (isWall down)))
                    (and  (or (isBox left) (isBoxGoal left))
                          (or (and (isWall left_up) (isWall up))
                              (and (isWall left_down) (isWall down))))
                    (and  (or (isBox right) (isBoxGoal right))
                          (or (and (isWall right_up) (isWall up))
                              (and (isWall right_down) (isWall down))))
                    (and  (or (isBox up) (isBoxGoal up))
                          (or (and (isWall left_up) (isWall left))
                              (and (isWall right_up) (isWall right))))
                    (and  (or (isBox down) (isBoxGoal down))
                          (or (and (isWall left_down) (isWall left))
                              (and (isWall right_down) (isWall right))))
                )))
            t)
        (t (box-in-corner-row-util s (rest row) r (+ 1 c)))))

(defun box-in-corner-util (state s r)
  (cond ((= 0 (length s)) nil)
        ((box-in-corner-row-util state (first s) r 0) t)
        (t (box-in-corner-util state (rest s) (+ 1 r)))))

(defun box-in-corner (s)
  (cond ((box-in-corner-util s s 0) 100)
        (t 0)))

;; (defun box-in-row (row)
;;   (numberp (position box row)))

;; (defun get-box-coords (s row)
;;   (cond ((= 0 (length s)) nil)
;;         (t (append (get-box-coords-row (first s))) (get-box-coords (rest s)))))

;; (defun h_keeper_box_manhattan_distance (s)
;;   (min (get-box-coords s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Some predefined problems.
 | Each problem can be visualized by calling (printstate <problem>). For example, (printstate p1).
 | Problems are ordered roughly by their difficulties.
 | For most problems, we also provide a number which indicates the depth of the optimal solution.
 | These numbers are located at the comments of the problems. For example, the first problem below has optimal solution depth 6.
 | As for the solution depth, any admissible heuristic must make A* return an optimal solution. So, the depths of the optimal solutions provided could be used for checking whether your heuristic is admissible.
 |
 | Warning: some problems toward the end are quite hard and could be impossible to solve without a good heuristic!
 | 
 |#
;(6)
(setq p1 '((1 1 1 1 1 1)
	   (1 0 3 0 0 1)
	   (1 0 2 0 0 1)
	   (1 1 0 1 1 1)
	   (1 0 0 0 0 1)
	   (1 0 4 0 4 1)
	   (1 1 1 1 1 1)))

;(15)
(setq p2 '((1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 1) 
	   (1 0 0 0 0 0 1) 
	   (1 0 0 2 1 4 1) 
	   (1 3 4 0 1 0 1)
	   (1 1 1 1 1 1 1)))

;(13)
(setq p3 '((1 1 1 1 1 1 1 1 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 0 0 2 0 3 4 1)
	   (1 0 0 0 1 0 0 0 1)
	   (1 0 4 0 1 0 0 0 1)
	   (1 1 1 1 1 1 1 1 1)))

;(17)
(setq p4 '((1 1 1 1 1 1 1)
	   (0 0 0 0 0 1 4)
	   (0 0 0 0 0 0 0)
	   (0 0 1 1 1 0 0)
	   (0 0 1 0 0 0 0)
	   (0 2 1 0 0 4 0)
	   (0 3 1 0 0 0 0)))

;(12)
(setq p5 '((1 1 1 1 1 1)
	   (1 1 0 0 1 1)
	   (1 0 0 0 0 1)
	   (1 4 2 2 4 1)
	   (1 0 0 0 4 1)
	   (1 1 3 1 1 1)
	   (1 1 1 1 1 1)))

;(13)
(setq p6 '((1 1 1 1 1 1 1 1)
	   (1 0 0 0 0 0 4 1)
	   (1 4 0 0 2 2 3 1)
	   (1 0 0 1 0 0 4 1)
	   (1 1 1 1 1 1 1 1)))

;(47)
(setq p7 '((1 1 1 1 1 1 1 1 1 1)
	   (0 0 1 1 1 1 4 0 0 3)
	   (0 0 0 0 0 1 0 0 0 0)
	   (0 0 0 0 0 1 0 0 1 0)
	   (0 0 1 0 0 1 0 0 1 0)
	   (0 2 1 0 0 0 0 0 1 0)
	   (0 0 1 0 0 0 0 0 1 4)))

;(22)
(setq p8 '((1 1 1 1 1 1)
	   (1 4 0 0 4 1)
	   (1 0 2 2 0 1)
	   (1 2 0 1 0 1)
	   (1 3 4 0 4 1)
	   (1 1 1 1 1 1)))

;(34)
(setq p9 '((1 1 1 1 1 1 1 1 1) 
	   (1 1 1 0 0 1 1 1 1) 
	   (1 0 0 0 0 0 2 0 1) 
	   (1 0 1 0 0 1 2 0 1) 
	   (1 0 4 4 4 1 3 0 1) 
	   (1 1 1 1 1 1 1 1 1)))

;(59)
(setq p10 '((1 1 1 1 1 0 0)
	    (1 4 0 0 1 1 0)
	    (1 3 2 0 0 1 1)
	    (1 1 0 2 0 0 1)
	    (0 1 1 0 2 0 1)
	    (0 0 1 1 0 0 1)
	    (0 0 0 1 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 4 1)
	    (0 0 0 0 1 1 1)))

;(51)
(setq p11 '((0 0 1 0 0 0 0)
	    (0 2 1 4 0 4 0)
	    (0 2 0 4 0 0 0)	   
	    (3 2 1 1 1 4 0)
	    (0 0 1 4 0 0 0)))

;(41)
(setq p12 '((1 1 1 1 1 0 0 0)
	    (1 0 0 4 1 0 0 0)
	    (1 2 1 0 1 1 1 1)
	    (1 4 0 0 0 0 0 1)
	    (1 0 0 5 0 5 0 1)
	    (1 0 5 0 1 0 1 1)
	    (1 1 1 0 3 0 1 0)
	    (0 0 1 1 1 1 1 0)))

;(78)
(setq p13 '((1 1 1 1 1 1 1 1 1 1)
	    (1 3 0 0 1 0 0 4 4 1)
	    (1 0 2 0 2 0 0 4 4 1)
	    (1 0 2 2 2 1 1 4 4 1)
	    (1 0 0 0 0 1 1 4 4 1)
	    (1 1 1 1 1 1 0 0 0 0)))

;(26)
(setq p14 '((0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 0 0 0 3 0 0 0 0 0 0 0)
	    (0 0 0 0 0 0 1 0 0 1 0 0 0 0 0 0)
	    (0 0 0 0 0 1 0 0 0 0 1 0 0 0 0 0)
	    (1 1 1 1 1 0 0 0 0 0 0 1 1 1 1 1)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 0 1 0 0 0 0)
	    (0 0 0 0 1 0 0 0 0 0 4 1 0 0 0 0)
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)	    
	    (0 0 0 0 1 0 2 0 0 0 4 1 0 0 0 0)
	    ))

;(?)
(setq p15 '((0 0 1 1 1 1 1 1 1 0)
	    (1 1 1 0 0 1 1 1 1 0)
	    (1 0 0 2 0 0 0 1 1 0)
	    (1 3 2 0 2 0 0 0 1 0)
	    (1 1 0 2 0 2 0 0 1 0)
	    (0 1 1 0 2 0 2 0 1 0)
	    (0 0 1 1 0 2 4 0 1 0)
	    (0 0 0 1 1 1 1 0 1 0)
	    (0 0 0 0 1 4 1 0 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 0 1 4 0 1)
	    (0 0 0 0 1 4 4 4 0 1)
	    (0 0 0 0 1 1 1 1 1 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#|
 | Utility functions for printing states and moves.
 | You do not need to understand any of the functions below this point.
 |#

;
; Helper function of prettyMoves
; from s1 --> s2
;
(defun detectDiff (s1 s2)
  (let* ((k1 (getKeeperPosition s1 0))
	 (k2 (getKeeperPosition s2 0))
	 (deltaX (- (car k2) (car k1)))
	 (deltaY (- (cadr k2) (cadr k1)))
	 )
    (cond ((= deltaX 0) (if (> deltaY 0) 'DOWN 'UP))
	  (t (if (> deltaX 0) 'RIGHT 'LEFT))
	  );end cond
    );end let
  );end defun

;
; Translates a list of states into a list of moves.
; Usage: (prettyMoves (a* <problem> #'goal-test #'next-states #'heuristic))
;
(defun prettyMoves (m)
  (cond ((null m) nil)
	((= 1 (length m)) (list 'END))
	(t (cons (detectDiff (car m) (cadr m)) (prettyMoves (cdr m))))
	);end cond
  );

;
; Print the content of the square to stdout.
;
(defun printSquare (s)
  (cond ((= s blank) (format t " "))
	((= s wall) (format t "#"))
	((= s box) (format t "$"))
	((= s keeper) (format t "@"))
	((= s goal) (format t "."))
	((= s boxgoal) (format t "*"))
	((= s keepergoal) (format t "+"))
	(t (format t "|"))
	);end cond
  )

;
; Print a row
;
(defun printRow (r)
  (dolist (cur r)
    (printSquare cur)    
    )
  );

;
; Print a state
;
(defun printState (s)
  (progn    
    (dolist (cur s)
      (printRow cur)
      (format t "~%")
      )
    );end progn
  )

;
; Print a list of states with delay.
;
(defun printStates (sl delay)
  (dolist (cur sl)
    (printState cur)
    (sleep delay)
    );end dolist
  );end defun


(defun test-h0 ()
  (time (a* p1 #'goal-test #'next-states #'h0))
  (time (a* p2 #'goal-test #'next-states #'h0))
  (time (a* p3 #'goal-test #'next-states #'h0))
  (time (a* p4 #'goal-test #'next-states #'h0))
  (time (a* p5 #'goal-test #'next-states #'h0))
  (time (a* p6 #'goal-test #'next-states #'h0))
  (time (a* p7 #'goal-test #'next-states #'h0))
  (time (a* p8 #'goal-test #'next-states #'h0))
  (time (a* p9 #'goal-test #'next-states #'h0))
  (time (a* p10 #'goal-test #'next-states #'h0))
  (time (a* p11 #'goal-test #'next-states #'h0))
  (time (a* p12 #'goal-test #'next-states #'h0))
  (time (a* p13 #'goal-test #'next-states #'h0))
  (time (a* p14 #'goal-test #'next-states #'h0))
  (time (a* p15 #'goal-test #'next-states #'h0))

  )

(defun test-h1 ()
  (time (a* p1 #'goal-test #'next-states #'h1))
  (time (a* p2 #'goal-test #'next-states #'h1))
  (time (a* p3 #'goal-test #'next-states #'h1))
  (time (a* p4 #'goal-test #'next-states #'h1))
  (time (a* p5 #'goal-test #'next-states #'h1))
  (time (a* p6 #'goal-test #'next-states #'h1))
  (time (a* p7 #'goal-test #'next-states #'h1))
  (time (a* p8 #'goal-test #'next-states #'h1))
  (time (a* p9 #'goal-test #'next-states #'h1))
  (time (a* p10 #'goal-test #'next-states #'h1))
  (time (a* p11 #'goal-test #'next-states #'h1))
  (time (a* p12 #'goal-test #'next-states #'h1))
  (time (a* p13 #'goal-test #'next-states #'h1))
  (time (a* p14 #'goal-test #'next-states #'h1))
  (time (a* p15 #'goal-test #'next-states #'h1))
  )

(defun test-hUID ()
  (time (a* p1 #'goal-test #'next-states #'h704925466))
  (time (a* p2 #'goal-test #'next-states #'h704925466))
  (time (a* p3 #'goal-test #'next-states #'h704925466))
  (time (a* p4 #'goal-test #'next-states #'h704925466))
  (time (a* p5 #'goal-test #'next-states #'h704925466))
  (time (a* p6 #'goal-test #'next-states #'h704925466))
  (time (a* p7 #'goal-test #'next-states #'h704925466))
  (time (a* p8 #'goal-test #'next-states #'h704925466))
  (time (a* p9 #'goal-test #'next-states #'h704925466))
  (time (a* p10 #'goal-test #'next-states #'h704925466))
  (time (a* p11 #'goal-test #'next-states #'h704925466))
  (time (a* p12 #'goal-test #'next-states #'h704925466))
  (time (a* p13 #'goal-test #'next-states #'h704925466))
  (time (a* p14 #'goal-test #'next-states #'h704925466))
  (time (a* p15 #'goal-test #'next-states #'h704925466))
  )