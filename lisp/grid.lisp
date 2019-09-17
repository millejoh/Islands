(in-package :islands)
;; (named-readtables:in-readtable rutils:rutils-readtable)

(defclass grid-map ()
  ((coords :accessor grid-map-coords)
   (data :reader grid-data :initform (make-hash-table))))

(defmethod map-cell ((m grid-map) coord)
  (let ((coord_r (round-coord coord)))
    (when (find coord_r (grid-map-coords m) :test #'coord-equal)
      (gethash (coord-hash coord_r) (grid-data m)))))

(defmethod set-map-cell ((m grid-map) coord value)
  (let ((coord_r (round-coord coord)))
    (when (find coord_r (grid-map-coords m) :test #'coord-equal)
      (setf (gethash (coord-hash coord_r) (grid-data m))
            value))))



;;;
;;; Hexagonal Coordinate Systems
;;;
;;; Adapted from https://www.redblobgames.com/grids/hexagons/implementation.html

(defstruct (hex-coord (:constructor make-hex-coord-qr (q r &aux (s (+ (- r) (- q)))))
                      (:constructor make-hex-coord-qrs (q r s))
                      (:constructor make-hex-coord (q r &aux (s (+ (- r) (- q))))))
  q
  r
  s)

(defmethod coord-hash ((h hex-coord))
  (let* ((q (q h))
         (r (r h))
         (tmp (+ q (/ (+ r 1) 2))))
    (+ q (* tmp tmp))))

(defun q (hc)
  (declare (inline))
  (hex-coord-q hc))

(defun r (hc)
  (declare (inline))
  (hex-coord-r hc))

(defun s (hc)
  (declare (inline))
  (hex-coord-s hc))

(defmethod round-coord ((h hex-coord))
  (let* ((hq (round (q h)))
         (hr (round (r h)))
         (hs (round (s h)))
         (qdiff (abs (- hq (q h))))
         (rdiff (abs (- hr (r h))))
         (sdiff (abs (- hs (s h)))))
    (if (and (> qdiff rdiff) (> qdiff sdiff))
        (setf hq (- (- hr) hs))
        (if (> rdiff sdiff)
            (setf hr (- (- hq) hs))
            (setf hs (- (- hq) hr))))
    (make-hex-coord-qrs hq hr hs)))

(defmethod coord-equal ((c1 hex-coord) (c2 hex-coord))
  (and (= (q c1) (q c2))
       (= (r c1) (r c2))
       (= (s c1) (s c2))))

(defmethod coord+ ((c1 hex-coord) (c2 hex-coord))
  (make-hex-coord-qrs (+ (q c1) (q c2))
                      (+ (r c1) (r c2))
                      (+ (s c1) (s c2))))

(defmethod coord- ((c1 hex-coord) (c2 hex-coord))
  (make-hex-coord-qrs (- (q c1) (q c2))
                      (- (r c1) (r c2))
                      (- (s c1) (s c2))))

(defmethod coord* ((c1 hex-coord) (k number))
  (make-hex-coord-qrs (* (q c1) k)
                      (* (r c1) k)
                      (* (s c1) k)))

(defun hex-length (h)
  (/ (+ (abs (q h)) (abs (r h)) (abs (s h))) 2))

(defun hex-distance (h1 h2)
  (hex-length (coord- h1 h2)))

(defparameter +hex-directions+
  #((make-hex-coord-qrs 1 0 -1)
    (make-hex-coord-qrs 1 -1 0)
    (make-hex-coord-qrs 0 - 1 1)
    (make-hex-coord-qrs -1 0 1)
    (make-hex-coord-qrs -1 1 0)
    (make-hex-coord-qrs 0 1 -1)))

(defun hex-direction (ix)
  (assert (and (> ix 0) (< ix 6)))
  (svref +hex-directions+ ix))

(defun hex-neighbor (hex direction)
  (coord+ hex (hex-direction direction)))

(defstruct (orientation (:constructor make-orientation (f0 f1 f2 f3 b0 b1 b2 b3 start-angle)))
  f0 f1 f2 f3
  b0 b1 b2 b3
  start-angle)

(defun f0 (o)
  (declare (inline))
  (orientation-f0 o))

(defun f1 (o)
  (declare (inline))
  (orientation-f1 o))

(defun f2 (o)
  (declare (inline))
  (orientation-f2 o))

(defun f3 (o)
  (declare (inline))
  (orientation-f3 o))

(defun b0 (o)
  (declare (inline))
  (orientation-b0 o))
(defun b1 (o)
  (declare (inline))
  (orientation-b1 o))
(defun b2 (o)
  (declare (inline))
  (orientation-b2 o))
(defun b3 (o)
  (declare (inline))
  (orientation-b3 o))

(defparameter *pointy-hex-layout*
  (make-orientation (sqrt 3.0)
                    (/ (sqrt 3.0) 2.0)
                    0.0
                    (/ 3.0 2.0)
                    (/ (sqrt 3.0) 2.0)
                    (/ -1.0 3.0)
                    0.0
                    (/ 2.0 3.0)
                    0.5))

(defparameter *flat-hex-layout*
    (make-orientation (/ 3.0 2.0)
                      0.0
                      (/ (sqrt 3.0) 2.0)
                      (sqrt 3.0)
                      (/ 2.0 3.0)
                      0.0
                      (/ -1.0 3.0)
                      (/ (sqrt 3.0) 2.0)
                      0.0))

(defclass hex-layout ()
  ((orientation :reader hex-layout-orientation :initarg :orientation)
   (size :reader hex-layout-size :initarg :size :type vec2)
   (origin :reader hex-layout-origin :initarg :origin :type vec2)))

(defun make-point-hex-layout (size origin)
  (make-instance 'hex-layout :orientation *pointy-hex-layout*
                             :size size
                             :origin origin))

(defmethod coord-to-pixel ((layout hex-layout) (h hex-coord))
  (with-slots ((o orientation) size origin) layout
    (let* ((px (* (+ (* (f0 o) (q h))
                      (* (f1 o) (r h)))
                  (x size)))
           (py (* (+ (* (f2 o) (q h))
                      (* (f3 o) (r h)))
                  (y size))))
      (vec2 (+ (x origin) px) (+ (y origin) py)))))

(defmethod pixel-to-coord ((layout hex-layout) (pixel vec2))
  (let ((pixel (flip-pixel-coord pixel)))
    (with-slots ((o orientation) size origin) layout
      (let* ((pt (vec2 (/ (- (x pixel) (x origin)) (x size))
                       (/ (- (y pixel) (y origin)) (y size))))
             (q (+ (* (b0 o) (x pt))
                   (* (b1 o) (y pt))))
             (r (+ (* (b2 o) (x pt))
                   (* (b3 o) (y pt)))))
        (make-hex-coord-qr q r)))))

(defun hex-corner-offset (layout corner)
  (with-slots (size (o orientation)) layout
    (let* ((angle (* 2.0 PI (/ (+ (orientation-start-angle o) corner)
                               6))))
      (vec2 (* (x size) (cos angle))
            (* (y size) (sin angle))))))

(defmethod coord-corners ((layout hex-layout) (h hex-coord))
  (let ((center (coord-to-pixel layout h)))
    (loop for i from 0 below 6
          collecting (let ((offset (hex-corner-offset layout i)))
                       (add center offset)))))

(defmethod flip-pixel-coord ((pixel vec2))
  (vec2 (x pixel)
        (- (gamekit:viewport-height) (y pixel))))

;;; Various map shapes

(defun make-circle-hex-map (radius)
  (let ((m (make-instance 'grid-map)))
    (setf (grid-map-coords m)
          (alexandria:flatten (loop for q from (- radius) upto radius by 1
                                    collecting (let ((r1 (max (- radius) (- (- q) radius)))
                                                     (r2 (min radius (+ (- q) radius))))
                                                 (loop for r from r1 upto r2
                                                       collecting (make-hex-coord-qrs q r (- (- q) r)))))))
    m))

;;; Drawing Maps/Layouts

(defmethod draw-grid-with-layout (map (layout hex-layout))
  (dolist (c (grid-map-coords map))
    (gamekit:draw-polygon (coord-corners layout c)
                          :fill-paint *white*
                          :stroke-paint *black*
                          :thickness 1.0)) )
