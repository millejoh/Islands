(cl:defpackage :islands
  (:use :cl :alexandria :trivial-gamekit)
  (:shadowing-import-from :trivial-gamekit
   :lerp)
  (:export play-game))
