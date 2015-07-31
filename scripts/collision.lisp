(defclass collision ()
   ((x :accessor collision-x
       :initform 0
       :initarg :x)
    (y :accessor collision-y
       :initform 0
       :initarg :y)))

(defmethod update ((self collision) (msg message))
    (format t "checking collision~%"))
