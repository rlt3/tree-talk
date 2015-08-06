(in-package :cl-user)
(use-package :tree-talk)

(defmacro handler (name super-list property-list)
    "A wrapper for classes so we can know the class name."
    `(defclass ,name ,super-list
        ,property-list))

(defmacro handle-message (name args handler &optional doc-string &rest body)
    "Creates a message-handler as a method for the handler's class."
    `(defmethod ,name ((self ,handler) (msg tree-talk::message) ,@args)
        ,doc-string
        (flet ((property (sym)
                    (slot-value self sym))
               (property-set! (sym value)
                    (setf (slot-value self sym) value))
               (think (title &rest data) 
                    (tree-talk::response #'tree-talk::post-think msg title data))
               (reply (title &rest data) 
                    (tree-talk::response #'tree-talk::post-reply msg title data))
               (broadcast (title &rest data) 
                    (tree-talk::response #'tree-talk::post-broadcast msg title data))
               (command (title &rest data) 
                    (tree-talk::response #'tree-talk::post-command msg title data)))
            ,@body)))

(defmacro helper (name args handler &optional doc-string &rest body)
    "Used to signify a procedure that isn't meant to handle messages."
    `(defmethod ,name ((self ,handler) ,@args)
        ,doc-string
        (flet ((property (sym)
                    (slot-value self sym))
               (property-set! (sym value)
                    (setf (slot-value self sym) value)))
            ,@body)))
