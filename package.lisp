;;;; package.lisp

(defpackage #:cl-foo
  (:use #:cl #:sdl2 #:alexandria)
  (:export #:main-loop
           ;; Matrices
           #:perspective-matrix
           #:look-at-matrix
           #:translation-matrix
           #:scale-matrix
           #:uniform-scale-matrix
           ;; Shaders
           #:read-shader
           #:shader-program
           #:with-shaders))
