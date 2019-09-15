(cl:defpackage :islands
  (:use :cl :iterate) ; :kr
  (:local-nicknames (:tgk :trivial-gamekit))
  (:import-from :trivial-gamekit :vec2 :vec3 :vec4 :x :y :z :w :mult :add :subt :div)
  (:shadowing-import-from :trivial-gamekit
                          :lerp)
  (:export initialize-game))
;; (cl:defpackage :islands
;;   (:use :cl :alexandria :trivial-gamekit :kr)
;;   (:shadowing-import-from :trivial-gamekit
;;    :lerp)
;;   (:export play-game))
