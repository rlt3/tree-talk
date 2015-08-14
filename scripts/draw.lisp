(handler draw ()
    ((x :initform 0 :initarg :x)
     (y :initform 0 :initarg :y)))

(handle-message update () draw
    "Output our coordinates."
    (format t "Drawing at (~D, ~D)~%" (property 'x) (property 'y)))

(handle-message location (coordinates) draw
    "Have our internals keep up with the location."
    (move self coordinates))

(helper move (coordinates) draw
    "Move our ourselves."
    (property-set! 'x (+ (property 'x) (car coordinates)))
    (property-set! 'y (+ (property 'y) (cadr coordinates)))
    self)
