(in-package :islands)

(multiple-value-bind (func table)
    (color-gradients:make-linear-gradient '(0 0) '(0 100) :color-1 '(0 0 255 255) :color-2 '(255 0 0 255))
  (defparameter *temperature-color-table* table)
  (defun get-temperature-color (temperature)
    (div (apply #'vec4 (funcall func 0 temperature)) 255.0)))

(multiple-value-bind (func table)
    (color-gradients:make-linear-gradient '(0 0) '(0 100) :color-1 '(0 0 0 255) :color-2 '(255 255 255 255))
  (defparameter *elevation-color-table* table)
  (defun get-elevation-color (elevation)
    (div (apply #'vec4 (funcall func 0 elevation)) 255.0)))
