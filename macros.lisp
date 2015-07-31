(defmacro create-class (name super-list property-list method-list)
    `(defclass ,name () ())
    (mapcar
        #'create-method
        (mapcar
            (lambda (m) (append name m))
            method-list)))

(defmacro handle-message (name class args &optional doc-string &rest body)
    `(defmethod ,name ((self ,class) (msg message) ,@args)
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

(defun me (macro)
    (macroexpand-1 macro))
