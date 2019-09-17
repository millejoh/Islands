(in-package :islands)

(defparameter *debug-frame-manager* nil)
(defvar *game-manager* nil)
(defvar *white* (gamekit:vec4 1 1 1 1))
(defvar *black* (gamekit:vec4 0 0 0 1))
(defvar *origin* (gamekit:vec2 0 0))
(defvar *current-box-position* (gamekit:vec2 0 0))
(defvar *t-color* (gamekit:vec4 0 0 0 1))
(defvar *hex-selection* nil)

(gamekit:defgame islands ()
  ((frame-manager :accessor islands-frame-manager :initform (make-instance 'box.frame:frame-manager :vsync-p nil :delta (/ 1 30.0)))
   (map :accessor islands-map :initform (make-circle-hex-map 10))
   (layout :accessor islands-map-layout :initform (make-instance 'hex-layout
                                                                 :orientation *flat-hex-layout*
                                                                 :size (vec2 16 16)
                                                                 :origin (vec2 200 200)))
   (world :accessor islands-world :initform (new-world 1000 1000)))
  (:viewport-width 800)
  (:viewport-height 600)
  (:fullscreen-p nil)
  (:prepare-resources t))


(defun bindings ()
  (gamekit:bind-button :mouse-left :pressed
                       (lambda ()
                         (process-mouse-down-event)))

  (gamekit:bind-button :mouse-left :released
                       (lambda ()
                         (process-mouse-up-event)))

  (gamekit:bind-cursor (lambda (x y)
                         (process-mouse-move-event x y))))

(defmethod gamekit:post-initialize ((game islands))
  (if *debug-frame-manager* (setf simple-logger:*current-level* :debug))
  (setf *game-manager* game)
  (prepare-resources)
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


(defun animate-engineer (time)
  (let* ((seconds (nth-value 0 (truncate time)))
         (frame (mod seconds 4)))
    (case frame
      (0 (get-resource '(:img :engineer :down0)))
      (1 (get-resource '(:img :engineer :down1)))
      (2 (get-resource '(:img :engineer :down2)))
      (3 (get-resource '(:img :engineer :down3))))))

(defun demo-draw-canvas (app)
  (declare (ignore app))
  (gamekit:draw-rect *current-box-position* 250 100 :stroke-paint *black*)
  (let ((relpos (gamekit:add *current-box-position* (gamekit:vec2 5 5)))
        (impos (gamekit:add *current-box-position* (gamekit:vec2 5 (- 100 20)))))
    (gamekit:with-pushed-canvas ()
      (draw-sprite (animate-engineer (real-time-seconds)) impos))
    (gamekit:draw-text (format nil "Hello world! ~C" #\lower_half_block)
                       relpos
                       :fill-color *t-color*
                       :font (font :unscii 10))
    (gamekit:draw-text (format nil "Last mouse down pos: ~A" *current-box-position*)
                       (gamekit:add relpos (vec2 0 15)))))

(defmethod gamekit:draw ((app islands))
  (box.frame:tick (islands-frame-manager app) 60 #'world-update)
  (demo-draw-canvas app)
  (draw-hex-grid app)
  ;; (draw-world (islands-world app))
  )


(defun world-update ()) ;; do nothing for now

(defun draw-hex-grid (app)
  (with-slots (map layout) app
    (draw-grid-with-layout map layout)))


(defun initialize-game ()
  (gamekit:start 'islands))

(defun set-debug-variables ()
  (setf *world* (islands-world *game-manager*))
  (setf *camera* (camera *world*)))

(defun restart-game ()
  (gamekit:stop)
  (gamekit:start 'islands))

(defparameter *frame-manager*
  (make-instance 'box.frame:frame-manager :delta (/ 1 30.0)))

(defparameter *mouse-down-p* nil)

(defun process-mouse-down-event ()
  (setq *mouse-down-p* t)
  (with-slots (map layout (pos gamekit::cursor-position)) (gamekit:gamekit)
    (setf *current-box-position* pos)
    (setf *hex-selection* (map-cell map (pixel-to-coord layout pos)))))

(defun process-mouse-up-event ()
  (setq *mouse-down-p* nil))

(defun process-mouse-move-event (x y)
  (when *mouse-down-p*
    (setf *current-box-position* (gamekit:vec2 x y))))
