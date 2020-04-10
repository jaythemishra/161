;; Jay Mishra
;; UID 704925466

;; For TREE-CONTAINS, I first checked if TREE was an atom. If it was, I checked if it was equal to N.
;; If tree wasn't an atom, I checked if N was equal to the second element of TREE, which is always an atom in the list form.
;; If N wasn't equal to this, I checked whether N existed in the left and right subtrees of TREE via recursive calls of TREE-CONTAINS.

;; For TREE-MIN, I first checked if TREE was an atom. If it was, I returned its value.
;; If not, I returned the value of a recursive call to TREE-MIN to the left subtree of TREE (ordered trees have the smallest value on the leftmost leaf).

;; For TREE-ORDER, I first checked if TREE was an atom. If so, I returned a list with it as the only element.
;; If not, I appended the results of recursive calls of TREE-ORDER on the first and third elements of TREE to a list with the second element of TREE as the only element.
;; (List versions of ordered TREEs are of the form (L m R), where L and R are ordered trees and m is an atom)

;; For SUB-LIST, I first checked if LEN was 0. If so, I returned NIL.
;; If not, I checked if START was greater than 0, meaning the first element of the list would not be in the sublist.
;; If so, I recursively called SUBLIST with the tail of L and START decremented by 1.
;; If both START was 0 and LEN was greater than 0, I would append the results of a recursive call to SUB-LIST with the tail of L and LEN decremented by 1 to the head of L.

;; for SPLIT-LIST, I first checked if the length of the list was even.
;; If so, I called SUB-LIST twice on L with the start set to 0 and the length set to the length of the list divided by two for the first call.
;; For the second call, I set START and LEN both to the length of the list divided by two.
;; If the length was odd, I called SPLIT-LIST on the tail of the LIST, and appended the first resulting list to the head of the original list.

;; For BTREE-HEIGHT, I recursively called BTREE-HEIGHT on the left and right subtrees, then returned 1 more than the larger of the two.




;; Determines whether the first argument N of type number appears in the second argument TREE, which is an ordered tree that could be a number or a list.
;; Returns t if N is in TREE, NIL otherwise.
(defun TREE-CONTAINS (N TREE)
  (cond ((atom TREE) (= N TREE))
        ((= N (second TREE)) t)
        (t  (or (TREE-CONTAINS N (first TREE))
                (TREE-CONTAINS N (third TREE))))))

;; Returns the smallest number in the ordered tree TREE, which could be either a number or a list.
(defun TREE-MIN (TREE)
  (cond ((atom TREE) TREE)
        (t (TREE-MIN (first TREE)))))

;; Returns a list containing the preorder traversal of the ordered tree TREE, which could be either a number or a list.
(defun TREE-ORDER (TREE)
  (cond ((atom TREE) (list TREE))
        (t (append  (list (second TREE))
                    (TREE-ORDER (first TREE))
                    (TREE-ORDER (third TREE))))))

;; Returns a sublist of list L that has length LEN and starts at index START. LEN and START are both numbers.
(defun SUB-LIST (L START LEN)
  (cond ((= LEN 0) NIL)
        ((> START 0) (SUB-LIST (rest L) (- START 1) LEN))
        (t (append  (list (first L))
                    (SUB-LIST (rest L) START (- LEN 1))))))

;; Returns a list of two lists that contain the elements of list L. The first element of the returned list is a list of length m.
;; The second element of the returned list of the length n. m - n = 1 or 0. When appended, the first and second list together equal L.
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

;; Returns the height of the binary tree TREE, which can either be a number or a list.
(defun BTREE-HEIGHT (TREE)
  (cond ((atom TREE) 0)
        (t  (let ((left_height (BTREE-HEIGHT (first TREE)))
                  (right_height (BTREE-HEIGHT (second TREE))))
              (cond ((> left_height right_height) (+ 1 left_height))
                    (t (+ 1 right_height)))))))

;; Returns a list containing a binary tree representation of the leaves in list LEAVES.
;; The internal nodes in the returned binary tree will have 0 or 1 more leaves in their left branches than their right branches.
(defun LIST2BTREE (LEAVES)
  (cond ((= 1 (length LEAVES)) (first LEAVES))
        ((= 2 (length LEAVES)) LEAVES)
        (t (let ((split_list (SPLIT-LIST LEAVES)))
              (list (LIST2BTREE (first split_list))
                    (LIST2BTREE (second split_list)))))))

;; Returns a list containing all the leaf nodes of the binary tree TREE, which can be either a number or a list.
(defun BTREE2LIST (TREE)
  (cond ((atom TREE) (list TREE))
        ((and (atom (first TREE))
              (atom (second TREE)))
            TREE)
        (t (append  (BTREE2LIST (first TREE))
                    (BTREE2LIST (second TREE))))))

;; Returns whether the two arguments E1 and E2 are equivalent. E1 and E2 can either be numbers or lists.
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
