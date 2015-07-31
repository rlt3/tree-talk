(load "utilities.lisp")
(load "macros.lisp")
(load "parse.lisp")
(load "tree.lisp")
(load "message.lisp")
(load "post.lisp")

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
    (load "treepost.lisp"))

; can set to message-send-debug for debugging purposes
(setf (symbol-function 'message-send) #'message-send-to)

(tree-load! *tree*)

(defun treepost (title &rest body)
    "The entry point to message a tree."
    (message-post 
        (make-message *tree* title body #'post-broadcast)))
