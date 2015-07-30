(defclass deboog () ())

(defmethod update ((self deboog) (self message)))

(defmethod start ((self deboog) (msg message))
    "When we receive the start message, update this object's location."
    (flet ((think (title &rest data) (response-think msg title data)))
        (think 'location 11 12)))
