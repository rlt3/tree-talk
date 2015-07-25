;   What if we made a macro that made this code:
;(script-class name (inherit-a b)
;    ((public 'foo)
;     (private 'bar)
;     (protected 'baz))
;    (method-name (arg-a b c) 
;        (check-stuff arg-a b c)
;        (think 'make-sound 'laugh)))
;
;
;   into this:
;(defclass name (inherit-a b)
;    ((foo :accessor name-foo :initarg :foo)
;     (bar :initarg :bar)
;     (baz :reader name-baz :initarg :baz)))
;
;(defmethod method-name ((self name msg message) arg-a b c)
;    (let 
;        ((think (lambda (m-type &rest data)
;                    (message-think msg m-type data))))
;        (check-stuff arg-a b c)
;        (think 'make-sound 'laugh)))

(defmacro create-method (classname &rest definition)
    "Create a class method without having to declare self and other misc
    procedures so that the scripter can focus just on scripting."
        (let ((name (car definition))
          (args (append (list (list 'self classname)) (cadr definition)))
          (body (caddr definition)))
          `(defmethod ,name ,args
             ,body)))

(defmacro create-class (name super-list property-list method-list)
    `(defclass ,name () ())
    (mapcar
        #'create-method
        (mapcar
            (lambda (m) (append name m))
            method-list)))

(defun private-property (object property-symbol)
    "Clarity function for private property access in classes."
    (slot-value object property-symbol))
