(handler deboog () ())

(handle-message start () 
    "When we receive the start message, update this object's location."
    (think 'location 11 12))
