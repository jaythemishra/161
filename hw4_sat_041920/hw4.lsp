;;;;;;;;;;;;;;
; Homework 4 ;
;;;;;;;;;;;;;;


; EXERCISE: Modify this function to decide satisifiability of delta.
; If delta is satisfiable, sat? returns a list of n integers that represents a model of delta,  
; otherwise it returns NIL. (See spec for details.)
; param n: number of variables in delta
; param delta: a CNF represented as a list of lists
(defun sat? (n delta)
  (backtracking n delta '()))

;; Reloads file
(defun reload()
  (load "hw4.lsp"))

;; Tests f1
(defun test1()
  (solve-cnf "./cnfs/f1/sat_f1.cnf"))

;; Tests f2
(defun test2()
  (solve-cnf "./cnfs/f2/sat_f2.cnf"))

;; Tests f3
(defun test3()
  (solve-cnf "./cnfs/f3/sat_f3.cnf"))

;; Tests f4
(defun test4()
  (solve-cnf "./cnfs/f4/sat_f4.cnf"))

;; Tests f5
(defun test5()
  (solve-cnf "./cnfs/f5/sat_f5.cnf"))

;; Removes clauses from delta that are already true based on current assignment
(defun simplify (assignment delta)
  (cond ((= 0 (length delta)) nil)
        ((simplify-clause assignment (first delta)) (simplify assignment (rest delta)))
        (t (cons (first delta) (simplify assignment (rest delta)))
  )))

;; Returns true if the given clause is true based on current assignment
(defun simplify-clause (assignment clause)
  (cond ((= 0 (length clause)) nil)
        (t (let* ((var (first clause))
                  (value (check-assignment var assignment)))
              (cond ((and (numberp value) (= value var)) t)
                    (t (simplify-clause assignment (rest clause))))))))

;; Returns true if the given assignment is valid according to delta
(defun valid (assignment delta)
  (cond ((= 0 (length delta)) t)
        ((= 0 (length (first delta))) nil)
        (t (let* ((clause (first delta))
                  (var (first clause))
                  (value (check-assignment var assignment)))
              (cond ((or (null value) (= value var))
                        (valid assignment (rest delta)))
                    (t (valid assignment (cons (rest clause) (rest delta)))))))))

;; Returns the value of a variable if it has already been assigned, nil otherwise
(defun check-assignment (val assignment)
  (let ((idx1 (position val assignment))
        (idx2 (position (- val) assignment)))
    (cond ((numberp idx1) val)
          ((numberp idx2) (- val))
          (t nil))))

;; The helper function that implements backtrack search
(defun backtracking (n delta assignment)
  (cond ((= n 0) assignment)
        (t (let* ((var1 n)
                  (var2 (- var1))
                  (new-assignment1 (cons var1 assignment))
                  (new-assignment2 (cons var2 assignment)))
              (cond ((valid new-assignment1 delta)
                      (let* ((result1 (backtracking (- n 1)
                                                    (simplify new-assignment1 delta)
                                                    new-assignment1)))
                        (cond ((null result1) 
                                (cond ((valid new-assignment2 delta)
                                        (let* ((result2 (backtracking (- n 1)
                                                                      (simplify new-assignment2 delta)
                                                                      new-assignment2)))
                                          (cond ((null result2) nil)
                                                (t result2))))
                                      (t nil)))
                              (t result1))))
                    (t (cond ((valid new-assignment2 delta)
                                        (let* ((result2 (backtracking (- n 1)
                                                                      (simplify new-assignment2 delta)
                                                                      new-assignment2)))
                                          (cond ((null result2) nil)
                                                (t result2))))
                                      (t nil))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Functions that help you parse CNF from files in folder cnfs/
; You need not modify any functions in this section
; Usage (solve-cnf <path-to-file>)
; e.g., (solve-cnf "./cnfs/f1/sat_f1.cnf")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun split-line (line)
  (if (equal line :eof)
      :eof
      (with-input-from-string (s line) (loop for x = (read s nil) while x collect x))))

(defun read-cnf (filename)
  (with-open-file (in filename)
    (loop for line = (split-line (read-line in nil :eof)) until (equal line :eof)
      if (equal 'p (first line)) collect (third line)      ; var count
      if (integerp (first line)) collect (butlast line)))) ; clause

(defun parse-cnf (filename)
  (let ((cnf (read-cnf filename))) (list (car cnf) (cdr cnf))))

; Following is a helper function that combines parse-cnf and sat?
(defun solve-cnf (filename)
  (let ((cnf (parse-cnf filename))) (sat? (first cnf) (second cnf))))

