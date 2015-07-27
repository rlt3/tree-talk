(load "parse.lisp")
(load "tree.lisp")
(load "message.lisp")
(load "macros.lisp")

;   TODO:
;       * Need to create our `talk' procedures, e.g. reply, command, think
;       * We only want our tree to load every script file once even if it
;       is used in many leaves. How do we accomplish this?

(defvar tree (make-tree tree-structure))
(defvar branch (car (branch-children tree)))
(defvar leaf (car (branch-leaves branch)))

(defun reload ()
    (load "treepost.lisp"))

(tree-load! tree)

(defun post (tree title &rest body)
    "The entry point to message a tree."
    (message-tree (make-message tree title body) tree))
