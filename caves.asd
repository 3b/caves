(asdf:defsystem :caves
  :description "cave gen test"
  :version "0.0.1"
  :author "Bart Botta <00003b at gmail.com>"
  :license "MIT"
  :depends-on (alexandria sb-cga cl-opengl 3bgl-shader 3b-glim/example/s)
  :serial t
  :components ((:file "shaders")
               (:file "package")
               (:file "caves")))
