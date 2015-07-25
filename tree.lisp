(load "macros.lisp")
(load "leaf.lisp")
(load "branch.lisp")
(load "parse.lisp")

(defmethod tree-load! ((self branch))
    "Load the entire tree's leaves."
    (branch-load! self)
    (mapcar #'tree-load! (branch-children self)))

(defmethod tree-serialize ((self branch))
    "Serialize the branch and it's children if any."
    (list 
        (branch-each-leaf self #'leaf-serialize)
        (branch-each-child self #'tree-serialize)))

(defmethod tree-message ((self branch) (msg message))
    "Send a message to the entire tree."
    (branch-message self msg)
    (branch-each-child self 
        (lambda (b) (tree-message b msg)))
    self)

(defun make-tree (serialized-tree)
    "Create and return the head of a tree from a serialized tree."
    (make-branch
        (parse-each-leaf serialized-tree (lambda (o) (apply #'make-leaf o)))
        (parse-each-child serialized-tree #'make-tree)))

(defun message-tree (tree message-type &rest data)
    "Ease-of-use function that automatically makes a message object for us."
    (tree-message tree (make-message tree tree message-type data)))

;   TODO:
;       * Need to create our `talk' procedures, e.g. reply, command, think
;       * We only want our tree to load every script file once even if it
;       is used in many leaves. How do we accomplish this?
;
;   I need to rethink our message data structure so that I may have each leaf
;   refer to its branch so procedures like think can work correctly. Right now
;   our message object gets set.

(defvar tree (make-tree tree-structure))
(defvar branch (car (branch-children tree)))
(defvar leaf (make-leaf "draw.lisp" 'draw '(:x 800 :y 600)))

(defun test ()
    (make-tree (tree-serialize branch)))

(defun reload ()
    (load "tree.lisp"))

(tree-load! tree)
