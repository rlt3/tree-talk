(load "utilities.lisp")
(load "macros.lisp")
(load "parse.lisp")
(load "leaf.lisp")
(load "branch.lisp")
(load "tree.lisp")
(load "message.lisp")
(load "treepost.lisp")

;   TODO:
;       * We only want our tree to load every script file once even if it
;       is used in many leaves. How do we accomplish this?
;       * How do we handle threading? Per message or per message list? Is our
;       data structure thread-safe?

(defvar *tree* (make-tree tree-structure))
(defvar *branch* (car (branch-children *tree*)))
(defvar *leaf* (car (branch-leaves *branch*)))

(defvar *update* (make-message *branch* 'update () #'post-think))
(defvar *location* (make-message *branch* 'location '(22 -400) #'post-think))

(defun me (macro)
    (macroexpand-1 macro))

(defun reload ()
    (load "tree-talk.lisp"))

; can set to message-send-debug for debugging purposes
(setf (symbol-function 'message-send) #'message-send-to)

(tree-load! *tree*)

(defun message-tree (title &rest body)
    "The entry point to message a tree."
    (treepost 
        (make-message *tree* title body #'post-broadcast)))

(defun message-tree-list (message-list)
    "Send a list of messages to the tree."
    (mapcar #'treepost message-list))
