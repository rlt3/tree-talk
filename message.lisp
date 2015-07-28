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
   (  from :reader message-from
           :initform ()
           :initarg :from)
   (method :reader message-method
           :initform ()
           :initarg :method)
   ( title :reader message-type
           :initform 'default
           :initarg :title)
   (  body :reader message-body
           :initform ()
           :initarg :body)))

(defmethod message-set-to! ((self message) to)
    (setf (slot-value self 'to) to))

(defmethod message-send ((self message) object)
    "Send the message."
    (handler-case
        (apply 
            #'funcall
            (append
                (list (message-type self)
                      object
                      self)
                (message-body self)))
        (condition (e) ())))

(defmethod message-leaf ((self message) (leaf leaf))
    "Message the leaf."
    (message-send self (leaf-root leaf)))

(defmethod message-branch ((self message) (branch branch))
    "Message the leaves of this branch."
    (message-set-to! self branch)
    (branch-each-leaf branch 
        (lambda (leaf) 
            (message-leaf self leaf))))

(defmethod message-tree ((self message) (branch branch))
    "Send a message to the tree from a branch recursively."
    (message-branch self branch)
    (branch-each-child branch
        (lambda (child) 
            (message-tree self child))))

(defmethod message-method-broadcast ((self message))
    "Send a message to the entire tree."
    (message-tree self (message-from self)))

(defmethod message-method-think ((self message))
    "A leaf messages the other leaves on its branch."
    (message-branch self (message-to self)))

(defmethod message-method-reply ((self message))
    "A branch replies directly to another branch."
    (message-branch self (message-from self)))

(defmethod message-method-command ((self message))
    "A branch messages its children."
    (branch-each-child (message-from self)
        (lambda (child) 
          (message-branch self child))))

(defun make-message (from title body method)
    (make-instance 'message 
        :from from 
        :title title 
        :body body 
        :method method))

(defmethod message-post ((self message))
    "The message dispatch procedure."
    (funcall
        (message-method self)
        self))

(defmethod message-think ((self message) title &rest body)
    "Have a leaf of a branch message the other leaves on the branch."
    (make-message (message-to self) title body #'message-method-think))
