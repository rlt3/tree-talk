(load "leaf.lisp")

;;; A branch is a collection of leaves.
;;;
;;; Branches are a traditional `node' of a tree. Branches have children, but
;;; it is the leaves that define them. Thinking of a branch as an object, the
;;; leaves are their properties.

(defclass branch ()
    ((  leaves :reader branch-leaves
               :initform ()
               :initarg :leaves)
     (children :reader branch-children
               :initform ()
               :initarg :children)))

(defmethod branch-each-side ((self branch) side procedure)
    "Do the procedure for a deviant of the branch."
    (mapcar
        procedure
        (funcall side self)))

(defmethod branch-each-child ((self branch) procedure)
    "Do the procedure for each child of the branch."
    (branch-each-side self #'branch-children procedure))

(defmethod branch-each-leaf ((self branch) procedure)
    "Do the procedure for each leaf of the branch."
    (branch-each-side self #'branch-leaves procedure))

(defmethod branch-add-leaf! ((self branch) leaf)
    "Add a leaf to the branch. The leaf is appended to the end."
    (setf 
        (slot-value self 'leaves)
        (append (branch-leaves self) (list leaf))))

(defmethod branch-load! ((self branch))
    "Load the leaves of an branch."
    (branch-each-leaf self #'leaf-load!))

(defun make-branch (leaves children)
  (make-instance 'branch :leaves leaves :children children))
