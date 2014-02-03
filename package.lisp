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
           #:uniform-vector
           #:string-to-shader
           #:shader-program
           #:with-shaders
           #:make-glsl-shader))
