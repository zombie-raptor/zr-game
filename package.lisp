;;;; package.lisp

(defpackage #:cl-foo
  (:use #:cl #:sdl2 #:cl-ppcre)
  (:export #:main-loop
           ;; Matrices
           #:perspective-matrix
           #:look-at-matrix
           #:translation-matrix
           #:scale-matrix
           #:uniform-scale-matrix
           ;; Graphics
           #:camera
           #:with-buffers
           #:with-sdl2
           #:with-vertex-attrib-array
           #:gl-array
           #:get-cube-elements
           #:get-cube-points
           ;; Shaders
           #:uniform-matrix
           #:string-to-shader
           #:shader-program
           #:with-shaders
           #:make-glsl-shader))
