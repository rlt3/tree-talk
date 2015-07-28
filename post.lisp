(defmethod post-to-leaf ((self message) (leaf leaf))
    "Message the leaf."
    (message-send self (leaf-root leaf)))

(defmethod post-to-branch ((self message) (branch branch))
    "Message the leaves of this branch."
    (message-set-recipient! self branch)
    (branch-each-leaf branch 
        (lambda (leaf) 
            (post-to-leaf self leaf))))

(defmethod post-to-branch-recursive ((self message) (branch branch))
    "Send a message to a tree from a branch recursively."
    (append 
        (post-to-branch self branch)
        (branch-each-child branch
            (lambda (child) 
                (post-to-branch-recursive self child)))))

(defmethod post-broadcast ((msg message))
    "Send a message to the entire tree."
    (post-to-branch-recursive msg (message-author msg)))

(defmethod post-think ((msg message))
    "A leaf messages the other leaves on its branch."
    (post-to-branch msg (message-author msg)))

(defmethod post-reply ((msg message))
    "A branch replies directly to another branch."
    (post-to-branch msg (message-to msg)))

(defmethod post-command ((msg message))
    "A branch messages its children."
    (branch-each-child (message-author msg)
        (lambda (child) 
          (post-to-branch msg child))))

(defmethod response-think ((old message) title &rest body)
    "Assemble the message for post-think."
    (message-set-recipient! 
        (make-message (message-to old) title body #'post-think)
        (message-author old)))