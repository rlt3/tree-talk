(load "macros.lisp")
(load "leaf.lisp")
(load "branch.lisp")
(load "parse.lisp")
(load "message.lisp")

(defmethod tree-load! ((self branch))
    "Load the entire tree's leaves."
    (branch-load! self)
    (mapcar #'tree-load! (branch-children self)))

(defmethod tree-serialize ((self branch))
    "Serialize the branch and it's children if any."
    (list 
        (branch-each-leaf self #'leaf-serialize)
        (branch-each-child self #'tree-serialize)))

(defun make-tree (serialized-tree)
    "Create and return the head of a tree from a serialized tree."
    (make-branch
        (parse-each-leaf serialized-tree (lambda (o) (apply #'make-leaf o)))
        (parse-each-child serialized-tree #'make-tree)))

;   TODO:
;       * Need to create our `talk' procedures, e.g. reply, command, think
;       * We only want our tree to load every script file once even if it
;       is used in many leaves. How do we accomplish this?

(defvar tree (make-tree tree-structure))
(defvar branch (car (branch-children tree)))
(defvar leaf (make-leaf "draw.lisp" 'draw '(:x 800 :y 600)))

(defun query ()
    (message-tree tree 'update))

(defun add ()
    (message-tree tree 'location 11 11))

(defun reload ()
    (load "tree.lisp"))

(tree-load! tree)
