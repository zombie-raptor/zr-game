;;;; cl-foo.asd

(asdf:defsystem #:cl-foo
  :serial t
  :description "This is just a sandbox for me to test out cl packages, SDL2, and OpenGL."
  :author "Michael Babich"
  :license "MIT"
  :depends-on (:sdl2
               :alexandria)
  :components ((:file "package")
               (:file "cl-foo")
               (:file "matrix-math")))
