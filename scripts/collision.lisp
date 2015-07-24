(defclass collision ()
   ((x :accessor collision-x
       :initform 0
       :initarg :x)
    (y :accessor collision-y
       :initform 0
       :initarg :y)))

(defmethod move ((self collision) a b)
    "(x + a, y + b) => (x, y)"
    (setf (collision-x self) (+ (collision-x self) a))
    (setf (collision-y self) (+ (collision-y self) b))
    (collision-check self))

(defmethod update ((self collision))
  (format t "checking collision~%"))
