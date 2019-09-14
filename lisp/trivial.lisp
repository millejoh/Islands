(in-package :islands)

(defparameter *ufont* nil)
(defvar *black* (gamekit:vec4 0 0 0 1))
(defvar *white* (gamekit:vec4 1 1 1 1))
(defvar *origin* (gamekit:vec2 0 0))
(defvar *current-box-position* (gamekit:vec2 0 0))
(defvar *t-color* (gamekit:vec4 0 0 0 1))
(defvar *hex* (make-hex 300 300 10))

(gamekit:defgame hello-gamekit ()
  ()
  (:viewport-width 800)
  (:viewport-height 600)
  (:fullscreen-p nil)
  (:prepare-resources t))

(gamekit:register-resource-package :keyword "~/Dropbox/Projects/Islands/lisp/_assets/")

(defun load-assets ()
  (gamekit:define-image :engineer "Engineer.png"))

(defun load-fonts ()
  (gamekit:define-font :unscii-font "unscii-16-full.ttf")
  (setf *ufont* (gamekit:make-font :unscii-font 32)))

(defun bindings ()
  (gamekit:bind-button :mouse-left :pressed
                       (lambda ()
                         (process-mouse-down-event)))

  (gamekit:bind-button :mouse-left :released
                       (lambda ()
                         (process-mouse-up-event)))

  (gamekit:bind-cursor (lambda (x y)
                         (process-mouse-move-event x y))))

(defmethod gamekit:post-initialize ((game hello-gamekit))
  (load-assets)
  (bindings))

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
  (unless *ufont* (load-fonts))
  (gamekit:draw-rect *current-box-position* 250 100 :stroke-paint *black*)
  (let ((relpos (gamekit:add *current-box-position* (gamekit:vec2 5 5)))
        (impos (gamekit:add *current-box-position* (gamekit:vec2 5 (- 100 20)))))
    (draw-hex-grid)
    (gamekit:draw-image impos :engineer :width 16 :height 16)
    (gamekit:draw-text ;(format nil "Hello world. ~C" #\lower_half_block)
     (format nil "Hello world.")
     relpos
     :fill-color *t-color*
     ;; :font *ufont*
     )))

(defun draw-hex-grid ()
  (let ((layout (make-instance 'hex-layout
                               :orientation *flat-hex-layout*
                               :size (vec2 10 10)
                               :origin (vec2 10 10)))
        (circle-map (make-circle-map 10)))
    (dolist (c (grid-map-coords circle-map))
      (gamekit:draw-polygon (coord-corners layout c)
                            :fill-paint *white*
                            :stroke-paint *black*
                            :thickness 1.0))))


(defun initialize-game ()
  (gamekit:start 'hello-gamekit))

(defun restart-game ()
  (gamekit:stop)
  (gamekit:start 'hello-gamekit))

(defparameter *frame-manager*
  (make-instance 'box.frame:frame-manager :delta (/ 1 30.0)))

(defparameter *mouse-down-p* nil)

(defun process-mouse-down-event ()
  (setq *mouse-down-p* t)
  (let ((gamekit (gamekit:gamekit)))
    (setf *current-box-position* (slot-value gamekit 'gamekit::cursor-position))))

(defun process-mouse-up-event ()
  (setq *mouse-down-p* nil))

(defun process-mouse-move-event (x y)
  (when *mouse-down-p*
    (setf *current-box-position* (gamekit:vec2 x y))))
