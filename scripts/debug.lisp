(defclass deboog () ())

(defmethod update ((self deboog) (self message)))

(defmethod start ((self deboog) (msg message))
    "When we receive the start message, update this object's location."
    (response-think msg 'location 11 12))
