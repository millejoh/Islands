(in-package :islands)
(named-readtables:in-readtable rutils-readtable)
#{:a 1}
(deftype climate (x)
  (member x '(:artic-alpine :cold :temperate :warm :tropical)))
(deftype biome (b)
  (member b  '(:snow :tundra :bare :scorched :taiga :shurbland :grassland
               :temperate-desert :temperate-rain-forest :temperate-deciduous-forest
               :tropical-mountain-forest :tropical-seasonal-forest
               :subtropical-desert :cold-desert :boreal-forest :hot-desert
               :savanna :tropical-dry-forest :tropical-evergreen-forest
               :thorn-forest :tropical-rain-forest :temperae-forest)))

(defparameter *biome-colors* (make-hash-table))

(let ((bc *biome-colors*))
  (setf (gethash :snow bc) (tcod:compose-color 248 248 248)
        (gethash :tundra bc) (tcod:compose-color 221 221 187)
        (gethash :bare bc) (tcod:compose-color 187 187 187)
        (gethash :scorched bc) (tcod:compose-color 153 153 153)
        (gethash :taiga bc) (tcod:compose-color 204 212 187)
        (gethash :shrubland bc) (tcod:compose-color 194 204 187)
        (gethash :grassland bc) (tcod:compose-color 192 212 170)
        (gethash :temperate-desert bc) (tcod:compose-color 228 232 202)
        (gethash :temperate-rain-forest bc) (tcod:compose-color 164 196 168)
        (gethash :tropical-rain-forest bc) (tcod:compose-color 156 187 169)
        (gethash :temperate-deciduous-forest bc) (tcod:compose-color 180 201 169)
        (gethash :tropical-seasonal-forest bc) (tcod:compose-color 169 204 164)
        (gethash :subtropical-desert bc) (tcod:compose-color 233 221 199)))

(defparameter biome_diagram
  '(
    ;; artic/alpine climate (below -5degC)
    (:tundra, :tundra, :tundra, :tundra, :tundra)
    ;; cold climate (-5 / 5 degC)
    (:cold_desert, :grassland, :boreal_forest, :boreal_forest, :boreal_forest)
    ;; temperate climate degC
    (:cold_desert, :grassland, :temperate_forest, :temperate_forest, :tropical_mountain_forest)
    ;; warm climate (15 - 20 degC)
    (:hot_desert, :savanna, :tropical_dry_forest, :tropical_evergreen_forest, :tropical_evergreen_forest)
    ;; tropical climate (above 20 degC)
    (:hot_desert, :thorn_forest, :tropical_dry_forest, :tropical_evergreen_forest, :tropical_evergreen_forest)))


;; Land Color Map
;;

(defconstant +sand-height+ 0.12)
(defconstant +grass-height+ 0.16)
(defconstant +rock-height+ 0.655)
(defconstant +snow-height+ 0.905)
(defconstant +color-key-max-sea+ (1- (round (* +sand-height+ 255))))
(defconstant +color-key-min-land+ (round (* +sand-height+ 255)))
(defconstant +key-index+
  (list
   0
   COLOR_KEY_MAX_SEA
   COLOR_KEY_MIN_LAND
   (round (* GRASS_HEIGHT 255))
   (round (+ (* GRASS_HEIGHT 255) 10))
   (round (*  ROCK_HEIGHT 255))
   (round (+ (* ROCK_HEIGHT  255)  10))
   (round (* SNOW_HEIGHT 255))
   (round (+ (* SNOW_HEIGHT 255) 10))
   255)

(defconstant +keyrgb-color-int+
  '((0 0 50)  ;; deep water
    (20 20 200)  ;; water-sand transition
    (134 180 101)  ;; sand
    (80 120 10)  ;; sand-grass transition
    (17 109 7)  ;; grass
    (30 85 12)  ;; grass-rock transisiton
    (64  70 20)  ;; rock
    (120 140 40)  ;; rock-snow transisiton
    (208 208 239)  ;; snow
    (255 255 255)))

;; Altitude Color MapChunk
;;

(defconstant +alt-indexes
  (list
   0 15 (round (* +sand-height+ 255) (1+ (round (* sand-height 255))))
   80 130 195 255))
(defconstant +altitudes '(-2000 -1000 -100 0 500 1000 2500 4000)) ;; In meters
(defconstant +altrgb-color-ints+
  '((24 165 255)
    (132 214 255)
    (49 149 44)
    (249 209 151)
    (165 148 24)
    (153 110 6)
    (172 141 138)))
