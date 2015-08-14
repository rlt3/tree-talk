(handler Input () ())

(defun input->coordinates (input)
    "Translate the input to the a direction."
    (case input
        (up    '(0 1))
        (down  '(0 -1))
        (right '(1 0))
        (left  '(-1 0))))

(handle-message input (direction) Input
    "When we receive the start message, update this object's location."
    (think 'location (input->coordinates direction)))
