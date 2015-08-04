(load "tree-talk.lisp")

(defun me (macro)
    (macroexpand-1 macro))

(defun reload ()
    (load "stage.lisp"))

(in-package :tree-talk)

(defmethod message-send ((self message) object)
    "Debug overwrite method for our `message-send' procedure."
        (funcall (message-title self) object self))
    ;(apply 
    ;    #'funcall
    ;    (append
    ;        (list (message-title self)
    ;              object
    ;              self)
    ;        (message-body self))))

(defvar *tree-structure*
    (list
        (list ())
        (list
            (list
                (list 
                      '("debug.lisp" deboog ())
                      '("draw.lisp" draw (:x 10 :y 15))
                      '("collision.lisp" collision ()))
                (list 
                    (list 
                        (list '("collision.lisp" collision ())
                              '("draw.lisp" draw (:x 800 :y 600)))
                        (list ()))))
            (list
                (list '("collision.lisp" collision ())
                      '("draw.lisp" draw (:x 40 :y 80)))
                (list ())))))

(defvar *tree* (make-tree *tree-structure*))

(export '*tree* :tree-talk)

;(defvar *branch* (car (branch-children *tree*)))
;(defvar *leaf* (car (branch-leaves *branch*)))
;
;(defvar *update* (make-message *branch* 'update () #'post-think))
;(defvar *location* (make-message *branch* 'location '(22 -400) #'post-think))

