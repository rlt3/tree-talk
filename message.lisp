;;; A message is sent to a method, not a node or set of nodes. This means that
;;; the author of the message doesn't know the recipients.
;;;
;;; Messages are sent to different `post offices'. Each post office sends the
;;; message in a different way. 

(defclass message ()
  ((  from :reader message-author :initform ()       :initarg :from)
   (method :reader message-method :initform ()       :initarg :method)
   ( title :reader message-title  :initform 'default :initarg :title)
   (  body :reader message-body   :initform ()       :initarg :body)

   ; stamps
   (recipient :accessor message-recipient :initarg :recipient)))

(defmethod message-stamp! (property-sym (self message) value)
    "Single entry for changing our message object."
    (setf (slot-value self property-sym) value)
    self)

(defmethod message-send ((self message) object)
    "Send the message to an object."
    (handler-case
        (apply 
            #'funcall
            (append
                (list (message-title self)
                      object
                      self)
                (message-body self)))
        (condition (e) ())))

(defun make-message (from title body method &rest stamps)
    "Make message with standard options and optionally add stamps which has to
    come in the form ':stamp-name stamp-value'"
    (apply
        #'make-instance
        (append
            (list 'message
                  :from from 
                  :title title 
                  :body body 
                  :method method)
            stamps)))
