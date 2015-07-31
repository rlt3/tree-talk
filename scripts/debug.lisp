(defclass deboog () ())

(defmethod update ((self deboog) (self message)))

(handle-message start deboog () 
    "When we receive the start message, update this object's location."
    (think 'location 11 12))
