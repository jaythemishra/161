; Determines whether the first argument N of type number appears in the second argument TREE, which is an ordered tree that could be a number or a list.
; Returns t if N is in TREE, NIL otherwise.
(defun TREE-CONTAINS (N TREE)
    (cond   ((atom TREE) (= N TREE))
            ((= N (second TREE)) t)
            (t (or (TREE-CONTAINS N (first TREE)) (TREE-CONTAINS N (third TREE))))))

; Returns the smallest number in the ordered tree TREE, which could be either a number or a list.
(defun TREE-MIN (TREE)
    (cond   ((atom TREE) TREE)
            (t (TREE-MIN (first TREE)))))

; Returns a list containing the preorder traversal of the ordered tree TREE, which could be either a number or a list.
(defun TREE-ORDER (TREE)
    (cond   ((atom TREE) (list TREE))
            (t (append (list (second TREE)) (TREE-ORDER (first TREE)) (TREE-ORDER (third TREE))))))

; Returns a sublist of list L that has length LEN and starts at index START. LEN and START are both numbers.
(defun SUB-LIST (L START LEN)
    (cond   ((= LEN 0) NIL)
            ((> START 0) (SUB-LIST (rest L) (- START 1) LEN))
            (t (append (list (first L)) (SUB-LIST (rest L) START (- LEN 1))))))