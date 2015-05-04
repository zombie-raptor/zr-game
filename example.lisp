;;;; This file uses the functionality provided by this game engine
;;;; library to make a very simple demo of its capabilities.

(in-package #:zr-game)

;;; Yes, I know this looks ugly. It's temporary. This is just an
;;; intermediate form that will be generated to make the source code
;;; look more natural.
(defparameter *shaders*
  (list (make-instance 'shader
                       :type :vertex-shader
                       :source '((:version 330)
                                 (:defvar position :vec4 :storage in :location 0)
                                 (:defvar view-matrix :mat4 :storage uniform)
                                 (:defvar projection-matrix :mat4 :storage uniform)
                                 (:defun main :void ()
                                         (:setf gl-position (:* projection-matrix
                                                                view-matrix
                                                                position)))))
        (make-instance 'shader
                       :type :fragment-shader
                       :source '((:version 330)
                                 (:defvar out-color :vec4 :storage out)
                                 (:defun main :void ()
                                         (:setf out-color (:vec4 0.3 0.3 0.5 1.0)))))))

;;;; Cubes

;;; Generates 6 faces on a cube from the 8 points on a cube of a given
;;; size with the cube center at OFFSET.
(defun get-cube-points (&key (size 0.5) (offset #(0.0 0.0 0.0 0.0)))
  (let ((point1 (map 'vector #'+ (vector (- size) (- size) (+ size) 1.0) offset))
        (point2 (map 'vector #'+ (vector (+ size) (- size) (+ size) 1.0) offset))
        (point3 (map 'vector #'+ (vector (+ size) (+ size) (+ size) 1.0) offset))
        (point4 (map 'vector #'+ (vector (- size) (+ size) (+ size) 1.0) offset))
        (point5 (map 'vector #'+ (vector (+ size) (+ size) (- size) 1.0) offset))
        (point6 (map 'vector #'+ (vector (- size) (+ size) (- size) 1.0) offset))
        (point7 (map 'vector #'+ (vector (+ size) (- size) (- size) 1.0) offset))
        (point8 (map 'vector #'+ (vector (- size) (- size) (- size) 1.0) offset)))
    (concatenate 'vector
                 point1 point2 point3 point4    ; front
                 point4 point3 point5 point6    ; top
                 point7 point8 point6 point5    ; back
                 point8 point7 point2 point1    ; bottom
                 point8 point1 point4 point6    ; left
                 point2 point7 point5 point3))) ; right

;;; Currently, this makes a cube to render on the screen.
(defun make-some-shapes (program buffers)
  (let ((triangle-points (reduce #'(lambda (x y) (concatenate 'vector x y))
                                 (map 'vector #'(lambda (x y) (map 'vector #'+ x y))
                                      (map 'vector #'(lambda (x) (make-array 6 :initial-element (* 4 x))) #(0 1 2 3 4 5))
                                      (make-array 6 :initial-element #(0 1 2 2 3 0))))))
    (make-instance 'vao
                   :program program
                   :array (get-cube-points :offset #(0.0 -4.0 -10.0 0.0))
                   :element-array triangle-points
                   :array-buffer (elt buffers 0)
                   :element-array-buffer (elt buffers 1)
                   :in-variable 'position)))

;;; Currently, configuration is done only through the arguments to
;;; this function. Eventually a menu and configuration files will be
;;; included.
(defun example (&key (width 1280) (height 720) (mouse-sensitivity 10) (title "OpenGL Rendering Test") (fullscreen nil))
  (sdl2:with-everything (:window (win :title title :w width :h height :fullscreen fullscreen) :gl gl)
    (with-buffers (buffers :count 2)
      (with-shaders (shaders program *shaders*)
        (sdl2:hide-cursor)
        (setup-gl :rgb-background #(0.0 0.1 0.01))
        (let ((main-camera (make-instance 'camera))
              (shapes (make-some-shapes program buffers)))
          (with-shader-program (program)
            (uniform-matrix program 'projection-matrix (perspective-matrix 45.0 (/ width height) 0.1 200.0)))
          (with-game-loop (win keydown-scancodes mouse-motion)
            (keyboard-move main-camera keydown-scancodes :speed 0.1)
            (mouse-move main-camera (elt mouse-motion 0) (elt mouse-motion 1) width height
                               :sensitivity mouse-sensitivity
                               :capture-window win)
            (with-shader-program (program)
              (uniform-matrix program 'view-matrix (get-matrix main-camera)))
            (draw shapes)))))))
