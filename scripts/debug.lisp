(defclass deboog () ())

(defmethod update ((self deboog) (self message)))

(defmethod start ((self deboog) (msg message))
    (response-think msg 'location 11 12))
