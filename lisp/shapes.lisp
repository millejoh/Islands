(in-package :islands)

(defclass shape ()
  ((center :accessor shape-center :initarg :center :type 'vec2)
   (size :accessor shape-size :initarg :size)
   (fill-color :accessor shape-fill-color :initarg :fill-color
               :initform (vec4 0.0 0.0 0.0 1.0))
   (stroke-paint :accessor shape-stroke-paint :initarg :stroke-paint
                 :initform (vec4 1.0 1.0 1.0 1.0))
   (thickness :accessor shape-line-thickness :initarg :line-thickness :initform 1.0)))


(defclass hex (shape)
  ((corners :reader shape-corners)
   (pointy :initarg :pointy :initform t)))

(defun make-hex (cx cy size)
  (make-instance 'hex
                 :center (vec2 cx cy)
                 :size size))


(defmethod corner ((h hex) i)
  (with-slots (center size pointy) h
    (let* ((angle_deg (- (* 60 i)
                          (if pointy 30 0)))
           (angle_rad (* angle_deg (/ PI 180))))
      (vec2 (+ (x center) (* size (cos angle_rad)))
            (+ (y center) (* size (sin angle_rad)))))))

(defmethod initialize-instance :after ((h hex) &rest args)
  (declare (ignore args))
  (setf (slot-value h 'corners) (loop for i from 0 upto 5
                                      collecting (corner h i))))

(defmethod draw-shape ((h hex))
  (with-slots (corners) h
    (gamekit:draw-polygon corners
                          :fill-paint (shape-fill-color h)
                          :stroke-paint (shape-stroke-paint h)
                          :thickness (shape-line-thickness h))))
