;;;; package.lisp

(defpackage #:cl-foo
  (:use #:cl #:sdl2 #:cl-opengl #:cl-glu #:alexandria)
  (:export #:main-loop))
