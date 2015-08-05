(defclass collision ()
   ((x :accessor collision-x
       :initform 0
       :initarg :x)
    (y :accessor collision-y
       :initform 0
       :initarg :y)))

(handle-message update () collision
    (format t "checking collision~%"))
