(asdf:defsystem #:zr-game
  :serial t
  :description "This is a game engine written in Common Lisp."
  :author "Michael Babich"
  :license "MIT"
  :depends-on (:sdl2 :cl-ppcre :sb-cga)
  :components ((:file "package")
               (:file "graphics")
               (:file "shader")
               (:file "game")
               (:file "example")))
