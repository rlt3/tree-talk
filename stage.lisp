(load "tree-talk.lisp")

(defvar tree-structure
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


(defvar *tree* (make-tree tree-structure))
(defvar *branch* (car (branch-children *tree*)))
(defvar *leaf* (car (branch-leaves *branch*)))

(defvar *update* (make-message *branch* 'update () #'post-think))
(defvar *location* (make-message *branch* 'location '(22 -400) #'post-think))

(defun me (macro)
    (macroexpand-1 macro))

(defun reload ()
    (load "scratch.lisp"))

(defmethod message-send-debug ((self message) object)
    "Debug overwrite method for our `message-send' procedure."
    (apply 
        #'funcall
        (append
            (list (message-title self)
                  object
                  self)
            (message-body self))))

(tree-load! *tree*)
