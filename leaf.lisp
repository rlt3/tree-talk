(load "message.lisp")

(defun apply-message (procedure message-type data-list)
    "A common form I've seen crop up with handling message data."
    (lambda (some-obj)
        (apply procedure
            (append (list some-obj message-type) data-list))))

(defun private-property (object property-symbol)
    "Clarity function for private property access in classes."
    (slot-value object property-symbol))

(defclass leaf ()
   ((filename  :reader leaf-filename 
               :initform "default"
               :initarg :filename)

    (class-sym :reader leaf-class-sym
               :initform 'default 
               :initarg :class-sym)

    ( env-vars :reader leaf-env-vars
               :initform '() 
               :initarg :env-vars)

    (object)))

(defmethod leaf-message ((self leaf) (msg message))
    "Message the leaf."
    (handler-case
        (message-send-to msg (private-property self 'object))
        (condition (e) self))
    self)

(defmethod leaf-load! ((self leaf))
    "Load our script file if needed and then load the script with the 
    environment vars. Not tied to constructor so that our object can get 
    reloaded dynamically."
    (load (concatenate 'string "scripts/" (leaf-filename self)))
    (setf 
        (slot-value self 'object) 
        (apply #'make-instance 
            (leaf-class-sym self)
            (leaf-env-vars self)))
    self)

(defmethod leaf-serialize ((self leaf))
    "Export lists and reload by applying those lists to make-leaf."
    (list (leaf-filename self)
          (leaf-class-sym self)
          (leaf-env-vars self)))

(defun make-leaf (f c v)
    "Make it easier to apply arg lists to make an instance."
    (make-instance 'leaf :filename f :class-sym c :env-vars v))
