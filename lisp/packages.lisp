(cl:defpackage :islands
  (:use :cl :alexandria :trivial-gamekit :kr)
  (:shadowing-import-from :trivial-gamekit
   :lerp)
  (:export play-game))
