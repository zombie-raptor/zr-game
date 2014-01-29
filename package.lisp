;;;; package.lisp

(defpackage #:cl-foo
  (:use #:cl #:sdl2)
  (:export #:main-loop
           ;; Matrices
           #:perspective-matrix
           #:look-at-matrix
           #:translation-matrix
           #:scale-matrix
           #:uniform-scale-matrix
           ;; Graphics
           #:with-buffers
           #:gl-array
           #:get-cube-elements
           #:get-cube-points
           ;; Shaders
           #:read-shader
           #:shader-program
           #:with-shaders
           #:make-glsl-shader))
