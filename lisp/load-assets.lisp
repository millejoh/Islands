(in-package :islands)
(defparameter *dawnlike-tile-size* 16)

(define-font :unscii "unscii-16-full.ttf" :priority 0)
(define-image :engineer "Engineer.png")

(add-resource '(:img :engineer :down0)
              (make-ss-sprite :engineer *dawnlike-tile-size* '(0 3) 1 1))
(add-resource '(:img :engineer :down1)
              (make-ss-sprite :engineer *dawnlike-tile-size* '(1 3) 1 1))
(add-resource '(:img :engineer :down2)
              (make-ss-sprite :engineer *dawnlike-tile-size* '(2 3) 1 1))
(add-resource '(:img :engineer :down3)
              (make-ss-sprite :engineer *dawnlike-tile-size* '(3 3) 1 1))

(defparameter *tilesheet-resources*
  '((:walls "Objects/Wall.png")
    (:floors "Objects/Floor.png")
    (:pits0 "Objects/Pit0.png")
    (:doors0 "Objects/Door0.png")
    (:decors0 "Objects/Decor0.png")
    (:grounds0 "Objects/Ground0.png")
    (:players0 "Characters/Player0.png")
    (:food "Items/Food.png")
    (:ores0 "Objects/Ore0.png")
    (:elementals0 "Characters/Elemental0.png")
    (:npcs0 "Characters/Misc0.png")
    (:long-weapons "Items/LongWep.png")
    (:avians0 "Characters/Avian0.png")))

(loop for ts in *tilesheet-resources*
      do (define-image (car ts) (cadr ts)))
