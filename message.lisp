;;; A message is simply a function call. A message, at the very end, is just
;;; invoking a function passed to title and passing the data to it. 
;;;
;;; The novel idea is that these function calls aren't just glorified methods.
;;; Any object, anywhere can listen for these messages and choose how to 
;;; respond or whether to respond at all. 
;;;
;;; Then there are specific ways of sending a message, like to itself, it's
;;; children, the children of its parent (excluding itself), etc. 
;;;
;;; The messages are asynchronous and a message doesn't require a response. Any
;;; response given would simply be a new message sent.

(defclass message ()
  ((    to :reader message-to
           :initform ()
           :initarg :to)
   (  from :reader message-author
           :initform ()
           :initarg :from)
   (method :reader message-method
           :initform ()
           :initarg :method)
   ( title :reader message-title
           :initform 'default
           :initarg :title)
   (  body :reader message-body
           :initform ()
           :initarg :body)))

(defmethod message-set-recipient! ((self message) to)
    (setf (slot-value self 'to) to)
    self)

(defmethod message-send ((self message) object)
    "Send the message."
    (handler-case
        (apply 
            #'funcall
            (append
                (list (message-title self)
                      object
                      self)
                (message-body self)))
        (condition (e) ())))

(defmethod message-post ((self message))
    "The message dispatch procedure."
    (flatten
        (funcall
            (message-method self)
            self)))

(defun make-message (from title body method)
    (make-instance 'message 
        :from from 
        :title title 
        :body body 
        :method method))
