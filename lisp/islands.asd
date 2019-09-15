(asdf:defsystem :islands
  :description ""
  :license "GPLv3"
  :version "0.1"
  :author "John Miller <millejoh@@macl.com>"
  ;; :depends-on (alexandria trivial-gamekit gamebox-frame-manager kr rutils tcod)
  :depends-on (alexandria trivial-gamekit gamebox-frame-manager defenum iterate color-gradients cl-colors) ;kr
  :serial t
  :components ((:file "packages")
               (:file "resources")
               (:file "colors")
               (:file "world")
               (:file "grid")
               (:file "trivial")
               (:file "load-assets")))
