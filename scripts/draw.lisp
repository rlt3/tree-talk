(defclass draw ()
   ((x :accessor draw-x
       :initform 0
       :initarg :x)
    (y :accessor draw-y
       :initform 0
       :initarg :y)))

;;
;; With draw + collision, in two different objects, have them walk through
;; collision with appropriate responses.
;;
;; You can start the sequence from something like 'start on a separate 
;; script.

(defmethod update ((self draw))
    (format t "Drawing at (~D, ~D)~%" (draw-x self) (draw-y self)))

(defmethod location ((self draw) x y)
    (_move self x y))

(defmethod _move ((self draw) a b)
    "(x + a, y + b) => (x, y)"
    (setf (draw-x self) (+ (draw-x self) a))
    (setf (draw-y self) (+ (draw-y self) b))
    self)

