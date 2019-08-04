(asdf:defsystem :islands
  :description ""
  :license "GPLv3"
  :version "0.1"
  :author "John Miller <millejoh@@macl.com>"
  :depends-on (alexandria trivial-gamekit gamebox-frame-manager kr tcod)
  :serial t
  :components ((:file "packages")
               (:file "trivial")))
