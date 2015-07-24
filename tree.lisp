(load "macros.lisp")
(load "leaf.lisp")
(load "branch.lisp")
(load "parse.lisp")

(defmethod tree-load! ((self branch))
    "Load the entire tree's leaves."
    (branch-load! self)
    (mapcar #'tree-load! (branch-children self)))

(defmethod tree-message ((self branch) (msg message))
    "Send a message to the entire tree."
    (branch-message self msg)
    (branch-each-child self 
        (lambda (b) (tree-message b msg))))

(defun make-tree (serialized-tree)
    "Create and return the head of a tree from a serialized tree."
    (make-branch
        (parse-each-leaf serialized-tree (lambda (o) (apply #'make-leaf o)))
        (parse-each-child serialized-tree #'make-tree)))

(defun message-tree (branch author message-type &rest data)
    "Ease-of-use function that automatically makes a message object for us."
    (tree-message branch (make-message branch author message-type data)))

;
;   TODO:
;       * Need to create our `talk' procedures, e.g. reply, command, think
;       * We only want our tree to load every script file once even if it
;       is used in many leaves. How do we accomplish this?

(defvar tree (make-tree tree-structure))
(defvar branch (car (branch-children tree)))
(defvar leaf (make-leaf "draw.lisp" 'draw '(:x 800 :y 600)))
