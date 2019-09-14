(in-package :islands)

(defclass world ()
  ((terrain :initarg :terrain :accessor terrain)
   (features :initarg :features :accessor features)
   (actors :initarg :actors :accessor actors)))

(defun new-world (width height)
  (let ((terrain (new-terrain width height)))
    (make-instance 'world
                   :terrain terrain
                   :features (make-hash-table)
                   :actors (make-hash-table))))

(defclass actor ()
  ((x :initform :x)
   (y :initform :y)
   (name :initform :name)
   (short-description :initform :short-description)
   (long-description :initform :long-description)))

(defun new-terrain (width height)
  (make-array (list width height 6) :element-type 'float :initial-element 0.0))

(defmacro terrain-elevation (terrain x y)
  `(aref ,terrain ,x ,y 0))

(defmacro terrain-temperature (terrain x y)
  `(aref ,terrain ,x ,y 1))

(defmacro terrain-ground-type (terrain x y)
  `(aref ,terrain ,x ,y 2))

(defmacro terrain-ground-moisture (terrain x y)
  `(aref ,terrain ,x ,y 3))

(defmacro terrain-foliage-type (terrain x y)
  `(aref ,terrain ,x ,y 4))

(defmacro terrain-foliage-density (terrain x y)
  `(aref ,terrain ,x ,y 5))

(defmacro with-terrain (terrain &rest body)
  (let ((ter-obj (gensym)))
    `(let ((,ter-obj ,terrain))
       (flet ((elevation (x y)
                (terrain-elevation ,ter-obj x y))
              (temperature (x y)
                (terrain-temperature ,ter-obj x y))
              (ground-type (x y)
                (terrain-ground-type ,ter-obj x y))
              (ground-moisture (x y)
                (terrain-ground-moisture ,ter-obj x y))
              (foliage-type (x y)
                (terrain-foliage-type ,ter-obj x y))
              (foliage-density (x y)
                (terrain-foliage-density ,ter-obj x y)))
         ,@body))))
