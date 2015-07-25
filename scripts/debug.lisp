(defclass deboog () ())

(defmethod update ((self deboog) (self message)))

(defmethod start ((self deboog) (msg message))
    (message-think msg 'update))
    ;(message-think msg 'location 11 12))
