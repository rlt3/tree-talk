(in-package :tree-talk)

(handler deboog () ())

(handle-message start () deboog
    "When we receive the start message, update this object's location."
    (think 'location 11 12))
