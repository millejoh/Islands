(in-package :islands)
(use-package :iterate)

(defparameter *default-tile-size* 16)

(defclass world ()
  ((terrain :initarg :terrain :accessor terrain)
   (features :initarg :features :accessor features)
   (actors :initarg :actors :accessor actors)
   (size :initarg :size :accessor size)
   (camera :initarg :camera :accessor camera :initform (make-instance 'camera))))

(defclass camera ()
  ((canvas-origin :initform (vec2 0 0) :initarg :display-frame
                  :documentation "Origin of the drawable portion of the camera view in the display canvas coordinate system. ")
   (view-center :accessor camera-view-center :initform (vec2 5 5) :initarg :pos
                :documentation "Position of tile camera is centered on.")
   (view-size :accessor camera-view-size :initform (list 10 10)  :initarg :width
              :initarg :view-size
              :documentation "Width and height, in tiles of camera viewport.")
   (tile-size :accessor camera-tile-size :initform *default-tile-size*
              :initarg :tile-size
              :documentation "Size of a tile, in pixels.")))

(defmethod move-camera ((w world) new-position)
  (with-slots ((c camera) (s size)) w
    (let ((cw (width (camera-view-size c)))
          (ch (height (camera-view-size c)))
          (ww (width s))
          (wh (height s))
          (x (x new-position))
          (y (y new-position)))
      (when (and (> (- x (/ cw 2)) 0)
                 (> (- y (/ ch 2)) 0)
                 (< (+ x (/ cw 2)) ww)
                 (< (+ y (/ ch 2)) wh))
        (setf (camera-view-center c) new-position)))))

(defmethod width ((size vec2))
  (x size))

(defmethod width ((size list))
  (first size))

(defmethod height ((size vec2))
  (y size))

(defmethod height ((size list))
  (second size))

(defmethod vec-as-list ((vec vec2))
  (list (x vec) (y vec)))

(defmethod vec-as-list ((vec vec3))
  (list (x vec) (y vec) (z vec)))

(defmethod vec-as-list ((vec vec4))
  (list (x vec) (y vec) (z vec) (w vec)))

(defun new-world (width height)
  (let ((terrain (new-terrain width height)))
    (make-instance 'world
                   :terrain terrain
                   :features (make-hash-table)
                   :actors (make-hash-table)
                   :size (vec2 width height))))

(defmethod draw-world ((world world))
  (with-slots ((c camera)) world
    (with-slots (view-center view-size tile-size canvas-origin) c
      (let ((frame (make-array (append view-size '(6))
                               :displaced-to (terrain world)
                               :displaced-index-offset
                               (array-row-major-index (terrain world)
                                                      (truncate (- (x view-center) (/ (width view-size) 2)))
                                                      (truncate (- (y view-center) (/ (height view-size) 2)))
                                                      0))))
        (draw-terrain frame canvas-origin tile-size)))))

(defun draw-terrain (terrain canvas-origin tile-size)
  (let ((ox (x canvas-origin))
        (oy (y canvas-origin)))
    (dotimes (i (array-dimension terrain 0))
      (dotimes (j (array-dimension terrain 1))
        (let ((dx (+ ox (* i tile-size)))
              (dy (+ oy (* j tile-size))))
          (draw-terrain-tile terrain i j (vec2 dx dy) tile-size))))))

(defun draw-terrain-tile (terrain i j origin tile-size &key (type :elevation))
  (let ((color (case type
                 (:elevation (get-elevation-color (terrain-elevation terrain i j)))
                 (:temperature (get-temperature-color (terrain-temperature terrain i j))))))
    (tgk:draw-rect origin tile-size tile-size :fill-paint color)))

(defclass actor ()
  ((x :initform :x)
   (y :initform :y)
   (name :initform :name)
   (short-description :initform :short-description)
   (long-description :initform :long-description)))


(defun new-terrain (width height)
  (make-array (list width height 6) :element-type 'float :initial-element 0.0))

(defun terrain-elevation (terrain x y)
  (aref terrain x y 0))

(defun set-terrain-elevation (terrain x y val)
  (setf (aref terrain x y 0) val))

(defsetf terrain-elevation set-terrain-elevation)

(defun terrain-temperature (terrain x y)
  (aref terrain x y 1))

(defun set-terrain-temperature (terrain x y val)
  (setf (aref terrain x y 1) val))

(defsetf terrain-temperature set-terrain-temperature)

(defun terrain-ground-type (terrain x y)
  (aref terrain x y 2))

(defun set-terrain-ground-type (terrain x y val)
  (setf (aref terrain x y 2) val))

(defsetf terrain-ground-type set-terrain-ground-type)

(defun terrain-ground-moisture (terrain x y)
  (aref terrain x y 3))

(defun set-terrain-ground-moisture (terrain x y val)
  (setf (aref terrain x y 3) val))

(defsetf terrain-ground-moisture set-terrain-ground-moisture)

(defun terrain-foliage-type (terrain x y)
  (aref terrain x y 4))

(defun set-terrain-foliage-type (terrain x y val)
  (setf (aref terrain x y 4) val))

(defsetf terrain-foliage-type set-terrain-foliage-type)

(defun terrain-foliage-density (terrain x y)
  (aref terrain x y 5))

(defun set-terrain-foliage-density (terrain x y val)
  (setf (aref terrain x y 0) val))

(defsetf terrain-foliage-density set-terrain-foliage-density)

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

(defun randomize-terrain-elevation (terrain &optional (max-height 100.0))
  (dotimes (i (array-dimension terrain 0))
    (dotimes (j (array-dimension terrain 1))
      (setf (terrain-elevation terrain i j) (random max-height)))))
