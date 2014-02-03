;;;; cl-foo.asd

(asdf:defsystem #:cl-foo
  :serial t
  :description "This is just a sandbox for me to test out cl packages, SDL2, and OpenGL."
  :author "Michael Babich"
  :license "MIT"
  :depends-on (:sdl2 :cl-ppcre)
  :components ((:file "package")
               (:file "math")
               (:file "shader")
               (:file "graphics")
               (:file "cl-foo")))
