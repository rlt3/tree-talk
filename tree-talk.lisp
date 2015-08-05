(defpackage :tree-talk
            (:use :common-lisp)
            (:export :self
                     :handler
                     :handle-message
                     :helper
                     :property
                     :property-set!
                     :think
                     :reply
                     :broadcast
                     :command
                     :make-tree
                     :tree-load!
                     :message-tree
                     :message-tree-list))

(in-package :tree-talk)

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

(defun message-tree (tree title &rest body)
    "The entry point to message a tree."
    (treepost 
        tree
        (make-message tree title body #'post-broadcast)))

(defun message-tree-list (tree message-list)
    "Send a list of messages to the tree."
    (mapcar 
        (lambda (msg) 
            (treepost tree msg)) 
        message-list))
