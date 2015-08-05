(load "tree-talk.lisp")

(defun me (macro)
    (macroexpand-1 macro))

(defun reload ()
    (load "stage.lisp"))

; setup our tree-structure and tree inside the tree-talk package
(in-package :tree-talk)
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

; and then export it so we can use it
(defvar *tree* (make-tree *tree-structure*))
(export '*tree* :tree-talk)

; and then setup userland with our loaded tree
(in-package :cl-user)
(use-package :tree-talk)
(tree-load! *tree*)
