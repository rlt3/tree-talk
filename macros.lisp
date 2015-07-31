;
;  These macros require one to use the `handler' macro to wrap their classes.
;  It sets the handler class name to use for the message handlers and helpers.
;  

(defvar *handler-class* ())

(defmacro handler (name super-list property-list)
    "A wrapper for classes so we can know the class name."
    (setq *handler-class* name)
    `(defclass ,name ,super-list
        ,property-list))

(defmacro handle-message (name args &optional doc-string &rest body)
    "Creates a message-handler as a method for the last handler's class. It 
    sets up the appropriate ease-of-use bindings for our responses and uses."
    `(defmethod ,name ((self ,*handler-class*) (msg message) ,@args)
        ,doc-string
        (flet ((property (sym)
                    (slot-value self sym))
               (think (title &rest data) 
                    (response-think msg title data))
               (reply (title &rest data) 
                    (response-reply msg title data))
               (broadcast (title &rest data) 
                    (response-broadcast msg title data))
               (command (title &rest data) 
                    (response-command msg title data)))
            ,@body)))

(defmacro helper (name args &optional doc-string &rest body)
    "Used to signify a procedure that isn't meant to handle messages. Creates a
    method with the last handler's class with ease-of-use bindings."
    `(defmethod ,name ((self ,*handler-class*) ,@args)
        ,doc-string
        (flet ((property (sym)
                    (slot-value self sym))
               (property-set (sym value)
                    (setf (slot-value self sym) value)))
            ,@body)))
