;;;; package.lisp

(defpackage #:cl-foo
  (:use #:cl #:sdl2 #:cl-ppcre #:sb-cga)
  (:export #:main-loop
           ;; Math
           #:get-cube-points
           #:get-cube-group
           ;; Game
           #:camera
           #:camera-matrix
           #:move-camera
           ;; Graphics
           #:with-sdl2
           #:with-game-loop
           #:with-buffers
           #:with-shaders
           #:with-shader-program
           #:with-vao
           #:make-array-buffer
           #:uniform-matrix
           #:uniform-vector
           #:perspective-matrix
           #:look-at-matrix
           ;; Shaders
           #:compile-shader
           #:shader-program
           #:make-glsl-shader
           #:shader
           #:shader-source
           #:shader-type))
