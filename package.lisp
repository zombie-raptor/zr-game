;;;; package.lisp

(defpackage #:cl-foo
  (:use #:cl #:sdl2 #:cl-ppcre)
  (:export #:main-loop
           ;; Math
           #:magnitude
           #:normalize
           #:cross-product
           #:dot-product
           #:perspective-matrix
           #:look-at-matrix
           #:translation-matrix
           #:scale-matrix
           #:matrix-product
           #:get-cube-points
           #:get-cube-group
           ;; Graphics
           #:camera
           #:camera-matrix
           #:with-buffers
           #:with-sdl2
           #:with-shaders
           #:with-shader-program
           #:with-vao
           #:make-array-buffer
           #:uniform-matrix
           #:uniform-vector
           ;; Shaders
           #:compile-shader
           #:shader-program
           #:make-glsl-shader
           #:shader
           #:shader-source
           #:shader-type))
