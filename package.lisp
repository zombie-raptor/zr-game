(defpackage #:zr-game
  (:use #:cl #:sdl2 #:cl-ppcre #:sb-cga)
  (:export #:example
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
           #:scancode-case
           #:with-buffers
           #:with-shaders
           #:with-shader-program
           #:draw-vao
           #:draw
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
