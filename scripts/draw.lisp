(handler draw ()
    ((x :initform 0 :initarg :x)
     (y :initform 0 :initarg :y)))

(handle-message update ()
    "Output our coordinates."
    (format t "Drawing at (~D, ~D)~%" (property 'x) (property 'y)))

(handle-message location (x y)
    "Have our internals keep up with the location."
    (move self x y))

(helper move (a b)
    "helper for..."
    (property-set 'x (+ (property 'x) a))
    (property-set 'y (+ (property 'y) b))
    self)
