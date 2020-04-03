; Jay Mishra
; UID 704925466
; For TREE-CONTAINS, I first checked if TREE was an atom. If it was, I checked if it was equal to N. If tree wasn't an atom, I did some stuff

; Determines whether the first argument N of type number appears in the second argument TREE, which is an ordered tree that could be a number or a list.
; Returns t if N is in TREE, NIL otherwise.
(defun TREE-CONTAINS (N TREE)
  (cond ((atom TREE) (= N TREE))
        ((= N (second TREE)) t)
        (t  (or (TREE-CONTAINS N (first TREE))
                (TREE-CONTAINS N (third TREE))))))

; Returns the smallest number in the ordered tree TREE, which could be either a number or a list.
(defun TREE-MIN (TREE)
  (cond ((atom TREE) TREE)
        (t (TREE-MIN (first TREE)))))

; Returns a list containing the preorder traversal of the ordered tree TREE, which could be either a number or a list.
(defun TREE-ORDER (TREE)
  (cond ((atom TREE) (list TREE))
        (t (append  (list (second TREE))
                    (TREE-ORDER (first TREE))
                    (TREE-ORDER (third TREE))))))

; Returns a sublist of list L that has length LEN and starts at index START. LEN and START are both numbers.
(defun SUB-LIST (L START LEN)
  (cond ((= LEN 0) NIL)
        ((> START 0) (SUB-LIST (rest L) (- START 1) LEN))
        (t (append  (list (first L))
                    (SUB-LIST (rest L) START (- LEN 1))))))

; Returns a list of two lists that contain the elements of list L. The first element of the returned list is a list of length m.
; The second element of the returned list of the length n. m - n = 1 or 0. When appended, the first and second list together equal L.
(defun SPLIT-LIST (L)
  (let ((len (length L)))
    (cond ((evenp len)
            (let* ( (second_len (/ len 2))
                    (first_len (- len second_len)))
              (list (SUB-LIST L 0 first_len)
                    (SUB-LIST L first_len second_len))))
          (t  (let ((even_split (SPLIT-LIST (rest L))))
                (list (append (list (first L))
                              (first even_split)) 
                      (second even_split)))))))

; Returns the height of the binary tree TREE, which can either be a number or a list.
(defun BTREE-HEIGHT (TREE)
  (cond ((atom TREE) 0)
        (t  (let ((left_height (BTREE-HEIGHT (first TREE)))
                  (right_height (BTREE-HEIGHT (second TREE))))
              (cond ((> left_height right_height) (+ 1 left_height))
                    (t (+ 1 right_height)))))))

; Returns a list containing a binary tree representation of the leaves in list LEAVES.
; The internal nodes in the returned binary tree will have 0 or 1 more leaves in their left branches than their right branches.
(defun LIST2BTREE (LEAVES)
  (cond ((= 1 (length LEAVES)) (first LEAVES))
        ((= 2 (length LEAVES)) LEAVES)
        (t (let ((split_list (SPLIT-LIST LEAVES)))
              (list (LIST2BTREE (first split_list))
                    (LIST2BTREE (second split_list)))))))

; Returns a list containing all the leaf nodes of the binary tree TREE, which can be either a number or a list.
(defun BTREE2LIST (TREE)
  (cond ((atom TREE) (list TREE))
        ((and (atom (first TREE))
              (atom (second TREE)))
            TREE)
        (t (append  (BTREE2LIST (first TREE))
                    (BTREE2LIST (second TREE))))))

; Returns whether the two arguments E1 and E2 are equivalent. E1 and E2 can either be numbers or lists.
(defun IS-SAME (E1 E2)
  (cond ((and (null E1)
              (null E2))
            t)
        ((and (atom E1)
              (atom E2))
            (= E1 E2))
        ((and (listp E1)
              (listp E2))
            (and  (IS-SAME (first E1) (first E2))
                  (IS-SAME (rest E1) (rest E2))))
        (t NIL)))
