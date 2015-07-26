(defclass message ()
  ((   sender :reader message-sender
              :initform ()
              :initarg :sender)
   (   author :reader message-author
              :initform ()
              :initarg :author)
   (     kind :reader message-type
              :initform 'default
              :initarg :kind)
   (     data :reader message-data
              :initform ()
              :initarg :data)
   (recipient :accessor message-recipient
              :initform ())))

(defun make-message (sender author kind data)
    (make-instance 'message 
            ;:sender sender 
            :author author 
            :kind kind 
            :data data))

(defmethod message-set-recipient! ((self message) recipient)
    (setf (message-recipient self) recipient))

(defmethod message-set-sender! ((self message) sender)
    (setf (slot-value self 'sender) sender))

(defmethod message-send ((self message))
    "Send the message."
    (apply 
        #'funcall
        (append
            (list (message-type self)
                  (message-recipient self)
                  self)
            (message-data self))))

(defmethod message-send-to ((self message) recipient)
    (message-set-recipient! self recipient)
    (handler-case
        (message-send self)
        (condition (e) self))
    self)

(defmethod message-leaf ((self message) (leaf leaf))
    "Message the leaf."
    (message-send-to self (leaf-petal leaf)))

(defmethod message-branch ((self message) (branch branch))
    "Message the leaves of this branch."
    (message-set-sender! self branch)
    (branch-each-leaf branch 
        (lambda (leaf) 
            (message-leaf self leaf))))

(defmethod message-tree ((self message) (branch branch))
    "Send a message to the entire tree."
    (message-branch self branch)
    (branch-each-child branch 
        (lambda (child) 
            (message-tree self child))))

(defun m (tree message-type &rest data)
    "Ease-of-use function that automatically makes a message object for us."
    (message-tree (make-message tree tree message-type data) tree))

(defmethod message-think ((self message) message-type &rest data)
    "Have a leaf of a branch message the other leaves on the branch."
    (let ((b (message-sender self)))
        (message-branch (make-message b b message-type data) b)))
