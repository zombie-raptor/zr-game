;;;; cl-foo.asd

(asdf:defsystem #:cl-foo
  :serial t
  :description "This is just a sandbox for me to test out cl packages, SDL2, and OpenGL."
  :author "Michael Babich"
  :license "Specify license here"
  :depends-on (:sdl2)
  :components ((:file "package")
               (:file "cl-foo")))

