(defpackage :tree-talk
            (:use :common-lisp)
            (:export :make-tree :tree-load! :tree-message :tree-message-list))

(in-package :tree-talk)

(load "utilities.lisp")
(load "parse.lisp")
(load "leaf.lisp")
(load "branch.lisp")
(load "tree.lisp")
(load "message.lisp")
(load "treepost.lisp")

(defun tree-message (tree title &rest body)
    "The entry point to message a tree."
    (treepost tree (make-message tree title body #'post-broadcast)))

(defun tree-message-list (tree message-list)
    "Send a list of messages to the tree."
    (mapcar (lambda (msg) 
                (treepost tree msg)) 
            message-list))
