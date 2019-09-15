
(in-package :islands)

(defparameter *resources* nil)

(defun define-font (name path &key (priority 0)) ; sort them based on prio and load in that order
  (gamekit:define-font name path)
  (push (cons priority name) *resources*))


(defun define-sound (name path &key (priority 0))
  (gamekit:define-sound name path)
  (push (cons priority name) *resources*))


(defun define-image (name path &key (priority 0))
  (gamekit:define-image name path)
  (push (cons priority name) *resources*))


;; https://gist.github.com/borodust/b75f964aa405c9d1ffa4be4654e3cbb1
(defun prepare-resources ()
  (loop :for r :in (mapcar #'cdr (sort (copy-list *resources*) #'< :key #'car))
        :for i :from 1
        :do (let ((r r))
              (bodge-concurrency:in-new-thread ("resource-loading")
                (sleep (/ i 2))
                (when (gamekit:gamekit) ; or it'll error if we have already exited.
                  (gamekit:push-action (lambda ()
                                     (gamekit:prepare-resources r))))))))

(let (fonts)
  (defun font (resource-name height)    ; at least i think it's height?
    (let ((tmp (assoc (list resource-name height) fonts :test #'equal)))
      (if tmp
          (cdr tmp)
          ;; it'd be nice if the condition was more specific than simple-error.
          (let ((f (handler-case (gamekit:make-font resource-name height) (simple-error () nil))))
            (if f
                (cdar (push (cons (list resource-name height) f)
                            fonts))
                gamekit::*font*)))))) ; or whatever we should default to.

;;;; Images --------------------------------------------------------------------
(defparameter *resource-table* (make-hash-table :test #'equalp))

;; can this be written clearer?
(defun add-resource (path object &optional (table *resource-table*))
  ;; if we're not at the end of path
  (if (and (consp path)
           (not (endp (cdr path))))
      ;; get the next ht or make one
      (let ((ht (multiple-value-bind (existing-ht foundp)
                    (gethash (car path) table)
                  (if foundp
                      existing-ht
                      (setf (gethash (car path) table) (make-hash-table :test #'equalp))))))
        (add-resource (rest path) object ht))
      ;; at the end, let's get the value
      (setf (gethash (if (atom path) path (first path))
                     table)
            object)))

(defun get-resource (path &optional (table *resource-table*))
  (if (and (consp path)
           (not (endp (cdr path))))
      (get-resource (rest path) (gethash (car path) table))
      (gethash (if (atom path) path (first path))
               table)))



;;;------------------------------------------------------------------------------
;;; Defining Global Resources
;;;------------------------------------------------------------------------------
(gamekit:register-resource-package
 :keyword (asdf:system-relative-pathname :islands "_assets/"))



;;;;----------------------------------------------------------------------------
;;;; SPRITE SHEETS
;;;;----------------------------------------------------------------------------


(defclass ss-sprite ()
  ((sprite-sheet
    :initarg :sprite-sheet
    :accessor ss-sprite-sprite-sheet)
   (origin
    :initarg :origin
    :accessor ss-sprite-origin)
   (width
    :initarg :width
    :accessor ss-sprite-width)
   (height
    :initarg :height
    :accessor ss-sprite-height)))

(defgeneric draw-sprite (s pos))

(defmethod draw-sprite ((s ss-sprite) pos)
  "Draws s at pos"
  (with-accessors ((ss     ss-sprite-sprite-sheet)
                   (origin ss-sprite-origin)
                   (width  ss-sprite-width)
                   (height ss-sprite-height))
      s
    (gamekit:draw-image pos
                        ss
                        :origin origin
                        :width  width
                        :height height)))


(defun make-ss-sprite (sprite-sheet tile-size origin w h)
  "`tile-size' is the size (in pixels) of a tile in the sprite sheet named `sprite-sheet'.
`origin' is a coordinate on the grid defined by `tile-size'. `w' and `h' are,
respectively, the size of the width and height of the sprite in tiles."
  (let ((x (elt origin 0))
        (y (elt origin 1)))
    (make-instance 'ss-sprite
                   :sprite-sheet sprite-sheet
                   :origin (gamekit:vec2 (* x tile-size) (* y tile-size))
                   :width  (1- (* w tile-size))
                   :height (1- (* h tile-size)))))
