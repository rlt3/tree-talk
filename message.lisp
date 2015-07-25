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

; BUG: can't set new message if you have data -- (message 'bad 1 0) can't be 
; updated to (message 'good 1 0) in the restart, but (message 'bad) can go to
; (message 'good).
(defun prompt-read (prompt)
    "After a label, get user input as string."
    (format *query-io* "~a: " prompt)
    (force-output *query-io*)
    (read-line *query-io*))

(defun prompt-for-new-message ()
    "Make a repl restart prompt so users can debug during runtime."
    (list (prompt-read "new message type")) )

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
    (restart-case
        (message-send self)
        (try-different-message (message-type)
            :interactive prompt-for-new-message
            (message-send-to
                (make-message 
                    (message-sender self)
                    (message-author self)
                    (eval (read-from-string message-type))
                    (message-data self))
                recipient))))

(defmethod message-think ((self message) message-type &rest data)
    (let ((b (message-sender self)))
        (branch-message b (make-message b b message-type data))))
