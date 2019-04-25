(in-package :cl-user)
(ql:quickload :trivial-gamekit)
(ql:quickload :gamebox-frame-manager)

(defpackage :hello-gamekit
  (:use :common-lisp :gamekit))

(in-package :hello-gamekit)

(defparameter *font* nil)
(defvar *black* (gamekit:vec4 0 0 0 1))
(defvar *origin* (gamekit:vec2 0 0))
(defvar *current-box-position* (gamekit:vec2 0 0))
(defvar *t-color* (gamekit:vec4 0 0 0 1))

(gamekit:defgame hello-gamekit ()
  ()
  (:viewport-width 1600)
  (:viewport-height 1200)
  (:fullscreen-p nil)
  (:prepare-resources t))

(gamekit:register-resource-package :keyword "~/Projects/Islands/lisp/_assets")

(defun load-assets ()
  (gamekit:define-image :engineer "Engineer.png")
  (gamekit:define-font :unscii-font "unscii-16-full.ttf"))

(defun load-fonts ()
  (setf *font* (gamekit:make-font :unscii-font 32)))

(defmethod post-initialize ((game hello-gamekit))
  (load-assets))

(defun real-time-seconds ()
  "Return seconds since certain point of time."
  (/ (get-internal-real-time) internal-time-units-per-second))

(defun update-position (position color time)
  (let* ((subseconds (nth-value 1 (truncate time)))
         (angle (* 2 pi subseconds)))
    (setf (gamekit:x color) (/ (mod time 120) 120)
          (gamekit:y color) (/ (mod (+ time 30) 120) 120)
          (gamekit:z color) (/ (mod (+ time 60) 120) 120))
    (setf (gamekit:x position) (+ 350 (* 100 (cos angle)))
          (gamekit:y position) (+ 350 (* 100 (sin angle))))))


(defmethod gamekit:draw ((app hello-gamekit))
  ;;(update-position *current-box-position* *t-color* (real-time-seconds))
  (gamekit:draw-rect *current-box-position* 250 100 :stroke-paint *black*)
  (let ((relpos (gamekit:add *current-box-position* (gamekit:vec2 5 5)))
        (impos (gamekit:add *current-box-position* (gamekit:vec2 5 (- 100 20)))))
    (gamekit:draw-image impos :engineer :width 16 :height 16)
    (gamekit:draw-text (format nil "Hello world. ~C" #\lower_half_block)
                       relpos
                       :fill-color *t-color*
                       :font *font*)))

(defun initialize-game ()
  (gamekit:start 'hello-gamekit))

(defun restart-game ()
  (gamekit:stop)
  (gamekit:start 'hello-gamekit))

(defparameter *frame-manager*
  (make-instance 'box.frame:frame-manager :delta (/ 1 30.0)))










