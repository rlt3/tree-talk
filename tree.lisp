;;; A tree is a collection of branches.

(defmethod tree-load! ((self branch))
    "Load the entire tree's leaves."
    (branch-load! self)
    (mapcar #'tree-load! (branch-children self)))

(defmethod tree-serialize ((self branch))
    "Serialize the branch and it's children if any."
    (list (branch-each-leaf self #'leaf-serialize)
          (branch-each-child self #'tree-serialize)))

(defun make-tree (serialized-tree)
    "Create and return the head of a tree from a serialized tree."
    (make-branch (parse-each-leaf serialized-tree 
                                  (lambda (o) (apply #'make-leaf o)))
                 (parse-each-child serialized-tree #'make-tree)))

