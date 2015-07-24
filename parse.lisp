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
;   
;   A tutorial:
;
;   car-ing an object (here we are using (car (object-children t-list))) gets us the
;   objects's components. 
;
;(car object) => (("collision.lisp" COLLISION NIL) ("draw.lisp" DRAW NIL) ("draw.lisp" DRAW (:X 10 :Y 15)))
;
;   This is a list of lists. Meaning we can mapcar through them and each car
;   will be a full component.
;
;(mapcar #'car (car object)) => ("collision.lisp" "draw.lisp" "draw.lisp")
;
;   Car-ing the car of each will return the first item in each component in 
;   the component list.
;
;   Cdr-ing the head will get the children of the head.
;
;(cdr object)  => ((((("collision.lisp" COLLISION NIL) ("draw.lisp" DRAW (:X 800 :Y 600))) (NIL))))
;
;   The car of the cdr (or cadr) will get the list of children.
;
;(cadr object) => (((("collision.lisp" COLLISION NIL) ("draw.lisp" DRAW (:X 800 :Y 600))) (NIL)))
;
;   Taking the car of the car of the cdr (caadr) will get the first object of
;   the children of the head. Likewise the caaadr will get the components of
;   the first child.
;   
;(caadr object)  => ((("collision.lisp" COLLISION NIL) ("draw.lisp" DRAW (:X 800 :Y 600))) (NIL))
;(caaadr object) => (("collision.lisp" COLLISION NIL) ("draw.lisp" DRAW (:X 800 :Y 600)))
;(car (caaadr object))  => ("collision.lisp" COLLISION NIL)
;(cadr (caaadr object)) => ("draw.lisp" DRAW (:X 800 :Y 600))
;
;   Since (cadr t-list) gives us the list of children much like t-list returns
;   us a list of objects, we can make this recursive.
;
;   The bare-minimum form we need to operate on it this:
;   (
;       ((component) (component) (component))
;       ((child) (child))
;   )
;   of which the children each themselves can flesh into the above and so on
;   and so forth.

(defun parse-left (tree)
    "Define which side is the components."
    (car tree))

(defun parse-right (tree)
    "Define which side is the children."
    (cadr tree))

(defun parse-each-side (tree side procedure)
    "Call a procedure on every element on one side of the object."
    (if (not (car (funcall side tree)))
        ()
        (mapcar
            procedure
            (funcall side tree))))

(defun parse-each-child (tree procedure)
    "For each of the children do a procedure."
    (parse-each-side tree #'parse-right procedure))

(defun parse-each-leaf (tree procedure)
    "For each of the leafs do a procedure."
    (parse-each-side tree #'parse-left procedure))
