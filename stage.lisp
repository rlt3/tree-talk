(load "tree-talk.lisp")
(load "macros.lisp")

(in-package :cl-user)
(use-package :tree-talk)

(defun reload ()
    (load "stage.lisp"))

(defvar *tree-structure*
    (list
        (list ())
        (list
            (list
                (list 
                      '("input.lisp" input ())
                      '("draw.lisp" draw (:x 10 :y 15)))
                (list 
                    (list 
                        (list '("draw.lisp" draw (:x 800 :y 600)))
                        (list ()))))
            (list
                (list '("draw.lisp" draw (:x 40 :y 80)))
                (list ())))))

(defvar *tree* (make-tree *tree-structure*))
(tree-load! *tree*)
