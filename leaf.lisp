;;; Leaves are what makes a branch be different from another branch.
;;;
;;; A leaf holds and iteracts with state that is kept hidden from all other
;;; leaves. The only way for any leaf root to interact with another is to have
;;; some external system act upon it.

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

    (   object :reader leaf-root
               :initform ())))

(defun in-list? (item l)
    (if (find item l) t ()))

(defmethod leaf-load-file! ((self leaf))
    (let ((files ()))
        (if (in-list? (leaf-filename self) files)
            (progn (setq files (append files (leaf-filename self)))
                   (load (concatenate 'string "scripts/" (leaf-filename self))))
            ())))

(defmethod leaf-load! ((self leaf))
    "Load our script file if needed and then load the script with the 
    environment vars. Not tied to constructor so that our object can get 
    reloaded dynamically."
    ;(leaf-load-file! self)
    (load (concatenate 'string "scripts/" (leaf-filename self)))
    (setf (slot-value self 'object) 
          (apply #'make-instance 
                (leaf-class-sym self)
                (leaf-env-vars self))))

(defmethod leaf-serialize ((self leaf))
    "Export lists and reload by applying those lists to make-leaf."
    (list (leaf-filename self)
          (leaf-class-sym self)
          (leaf-env-vars self)))

(defun make-leaf (f c v)
    "Make it easier to apply arg lists to make an instance."
    (make-instance 'leaf :filename f :class-sym c :env-vars v))
