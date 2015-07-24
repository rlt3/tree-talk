(defvar tree-structure
    (list
        (list ())
        (list
            (list
                (list '("collision.lisp" collision ())
                      '("draw.lisp" draw ())
                      '("draw.lisp" draw (:x 10 :y 15)))
                (list 
                    (list (list '("collision.lisp" collision ())
                              '("draw.lisp" draw (:x 800 :y 600)))
                        (list ()))))
            (list
                (list '("collision.lisp" collision ())
                      '("draw.lisp" draw (:x 40 :y 80)))
                (list ())))))

#| 
 
  The form of our data structure is ( () () ). So, it is a list that has two
lists inside. Knowing this, we can define how to get to each `side' and use
those definitions to define their meaning.

|#

(defun parse-left (form)
    (car form))

(defun parse-right (form)
    (cadr form))

(defun parse-each-side (form side procedure)
    "Call a procedure on every element on one side of the object."
    (if (not (car (funcall side tree)))
        ()
        (mapcar
            procedure
            (funcall side form))))

(defun parse-each-child (form procedure)
    "For each of the children do a procedure."
    (parse-each-side form #'parse-right procedure))

(defun parse-each-leaf (form procedure)
    "For each of the leafs do a procedure."
    (parse-each-side form #'parse-left procedure))
