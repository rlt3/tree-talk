(defun flatten (l)
    "Collect every element from a list of nested lists."
    (cond ((null l) nil)
          ((atom l) (list l))
          (t (loop for a in l appending (flatten a)))))

(defun not-messagep? (maybe-message)
    "Returns true if not a message. False if it is a message."
    (cond ((eq 'message (type-of maybe-message)) ())
          (t t)))
