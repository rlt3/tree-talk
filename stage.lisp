(load "tree-talk.lisp")

(in-package :cl-user)
(use-package :tree-talk)

(defun me (macro)
    (macroexpand-1 macro))

(defun reload ()
    (load "stage.lisp"))

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
(tree-load! *tree*)
