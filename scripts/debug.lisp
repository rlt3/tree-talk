(defclass deboog ())

(defmethod start ((self deboog) (msg message))
  ((message-think msg 'location 11 12)))
