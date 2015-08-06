;;; The `post office' of the tree -- treepost.

(defmethod post-send ((self message) object)
    "Send the message to an object."
    (handler-case
        (apply #'funcall
            (append (list (message-title self) object self)
                    (message-body self)))
        (condition (e) ())))

(defun post-to-leaf (message leaf)
    "Message the leaf."
    (post-send message (leaf-root leaf)))

(defun post-to-branch (message branch)
    "Message the leaves of this branch."
    (message-stamp! 'recipient message branch)
        (branch-each-leaf branch 
                         (lambda (leaf) 
                             (post-to-leaf message leaf))))

(defun post-to-branch-recursive (message branch)
    "Send a message to a tree from a branch recursively."
    (append (post-to-branch message branch)
            (branch-each-child branch
                              (lambda (child) 
                                  (post-to-branch-recursive message child)))))

(defun treepost (tree msg)
    "Dispatch the message via its method."
    (flatten (funcall (message-method msg) tree msg)))

;; Methods in which we send our messages.

(defun post-broadcast (tree message)
    "Send a message to the entire tree."
    (post-to-branch-recursive message tree))

(defun post-think (tree message)
    "A leaf messages the other leaves on its branch."
    (post-to-branch message (message-author message)))

(defun post-reply (tree message)
    "A branch replies directly to another branch."
    (post-to-branch message (message-recipient message)))

(defun post-command (tree message)
    "A branch messages its children."
    (branch-each-child (message-author message)
                       (lambda (child) 
                           (post-to-branch message child))))

(defun response (method old-message title body)
    "Create a response to the old message sent by a method."
    (apply #'make-message
           (append (list (message-recipient old-message) title body method)
                   (cond ((eq method #'post-reply) 
                             (list :recipient (message-author old-message))
                         (t '()))))))
