(defclass message ()
  ((sender :reader message-sender
           :initform 0
           :initarg :sender)
   (author :reader message-author
           :initform 0
           :initarg :author)
   (  kind :reader message-type
           :initform 'default
           :initarg :kind)
   (  data :reader message-data
           :initform ()
           :initarg :data)))

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

(defmethod message-send-to ((msg message) receiver)
    (restart-case
        (apply #'funcall (append (list (message-type msg) receiver)
                                 (message-data msg)))
        (try-different-message (msg)
            :interactive prompt-for-new-message
            (send 
                object 
                (eval (read-from-string msg))))))
