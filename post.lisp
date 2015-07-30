(defun post-to-leaf (message leaf)
    "Message the leaf."
    (message-send message (leaf-root leaf)))

(defun post-to-branch (message branch)
    "Message the leaves of this branch."
    (message-stamp! 'recipient message branch)
    (branch-each-leaf branch 
        (lambda (leaf) 
            (post-to-leaf message leaf))))

(defun post-to-branch-recursive (message branch)
    "Send a message to a tree from a branch recursively."
    (append 
        (post-to-branch message branch)
        (branch-each-child branch
            (lambda (child) 
                (post-to-branch-recursive message child)))))

;; Methods in which we send our messages.

(defun post-broadcast (message)
    "Send a message to the entire tree."
    (post-to-branch-recursive message *tree*))

(defun post-think (message)
    "A leaf messages the other leaves on its branch."
    (post-to-branch message (message-author message)))

(defun post-reply (message)
    "A branch replies directly to another branch."
    (post-to-branch message (message-recipient message)))

(defun post-command (message)
    "A branch messages its children."
    (branch-each-child (message-author message)
        (lambda (child) 
            (post-to-branch message child))))

;; Procedures which compose the responses as messages using the methods above.

(defun response-broadcast (old-message title &rest body)
    "Assemble the message for post-broadcast."
    (make-message (message-recipient old-message) title body #'post-broadcast))

(defun response-reply (old-message title &rest body)
    "Assemble the message for post-reply."
    (make-message (message-recipient old-message) title body #'post-reply
        :recipient (message-author old-message)))

(defun response-think (old-message title &rest body)
    "Assemble the message for post-think."
    (make-message (message-recipient old-message) title body #'post-think))

(defun response-command (old-message title &rest body)
    "Assemble the message for post-command."
    (make-message (message-recipient old-message) title body #'post-command))
