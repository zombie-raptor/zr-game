;;;; cl-foo.asd

(asdf:defsystem #:cl-foo
  :serial t
  :description "This is just a sandbox for me to test out SDL2 and OpenGL."
  :author "Michael Babich"
  :license "MIT"
  :depends-on (:sdl2 :cl-ppcre :sb-cga)
  :components ((:file "package")
               (:file "shader")
               (:file "graphics")
               (:file "game")
               (:file "cl-foo")))
