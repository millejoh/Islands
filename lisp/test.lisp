(in-package :islands)
(use-package :rutils.iter)

(defparameter *hex* nil)
(defparameter *corners* nil)

(setf *hex* (make-instance 'hex :center (vec2 0.0 0.0) :size 1.0))


(setf *corners*
      (loop for i from 0 upto 5
            collecting (corner *hex* i :pointy t)))
