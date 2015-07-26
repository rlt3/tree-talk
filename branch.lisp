(defclass branch ()
    ((  leaves :reader branch-leaves
               :initform ()
               :initarg :leaves)
     (children :reader branch-children
               :initform ()
               :initarg :children)))

(defun make-branch (leaves children)
  (make-instance 'branch :leaves leaves :children children))

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

(defmethod branch-message ((self branch) (msg message))
    "Message the leaves of this branch."
    ;(message-set-sender! msg self)
    (branch-each-leaf self (lambda (l) (leaf-message l msg))))

(defmethod branch-load! ((self branch))
    "Load the leaves of an branch."
    (branch-each-leaf self #'leaf-load!))
