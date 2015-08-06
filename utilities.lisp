(defun flatten (l)
    "Collect every element from a list of nested lists."
    (cond ((null l) nil)
          ((atom l) (list l))
          (t (loop for a in l appending (flatten a)))))
