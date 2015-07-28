(load "utilities.lisp")
(load "macros.lisp")
(load "parse.lisp")
(load "tree.lisp")
(load "message.lisp")
(load "post.lisp")

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

(defvar start (make-message branch 'start () #'post-broadcast))

(defun treepost (tree title &rest body)
    "The entry point to message a tree."
    (message-post 
        (make-message tree title body #'post-broadcast)))
