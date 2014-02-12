;;;; package.lisp

(defpackage #:zr-game
  (:use #:cl #:sdl2 #:cl-ppcre #:sb-cga)
  (:export #:main-loop
           ;; Cubes
           #:get-cube-points
           #:get-cube-group
           ;; Cameras
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
           #:compile-gl-shader
           #:link-gl-program
           #:make-array-buffer
           #:uniform-matrix
           #:uniform-vector
           #:perspective-matrix
           #:look-at-matrix
           ;; Shaders
           #:make-glsl-shader
           #:shader
           #:shader-source
           #:shader-type))
