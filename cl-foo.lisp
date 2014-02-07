;;;; This file uses the functionality provided by this game engine
;;;; library to make a very simple demo of its capabilities.

(in-package #:cl-foo)

(defparameter *shaders*
  (list (make-instance 'shader
                       :type :vertex-shader
                       :source '((:defvar position :vec4 :storage in :location 0)
                                 (:defvar offset :vec4 :storage uniform)
                                 (:defvar view-matrix :mat4 :storage uniform)
                                 (:defvar projection-matrix :mat4 :storage uniform)
                                 (:defun main :void ()
                                         (:setf gl-position (:* projection-matrix
                                                                view-matrix
                                                                (:+ position offset))))))
        (make-instance 'shader
                       :type :fragment-shader
                       :source '((:defvar out-color :vec4 :storage out)
                                 (:defun main :void ()
                                         (:setf out-color (:vec4 0.5 0.5 1.0 1.0)))))))

(defun main-loop (&key (width 1280) (height 720) (title "OpenGL Rendering Test") (fullscreen nil))
  (with-sdl2 (window :title title :width width :height height)
    ;; In my experience, fullscreen only works properly if the
    ;; resolution is the same as your monitor's resolution.
    (if fullscreen (sdl2:set-window-fullscreen window 1))
    (with-buffers (buffers :count 2)
      (with-shaders (shaders program *shaders*)
        (let ((camera (make-instance 'camera))
              (array-buffer (elt buffers 0))
              (element-array-buffer (elt buffers 1))
              (cube-points (get-cube-group-points 10 10 10 :offset #(0.0 -4.0 -10.0 0.0)))
              (cube-elements (get-cube-elements (expt 10 3))))
          (with-shader-program (program)
            (uniform-matrix program 'projection-matrix (perspective-matrix 45.0 (/ width height) 0.1 100.0))
            (uniform-vector program 'offset #(1.0 -2.0 -10.0 0.0))
            (make-array-buffer :array-buffer array-buffer :float cube-points)
            (make-array-buffer :element-array-buffer element-array-buffer :unsigned-short cube-elements))
          (with-game-loop (window #'(lambda (scancode) (move-camera camera scancode)))
            (with-vao (program array-buffer element-array-buffer 0 4 (length cube-elements) :float)
              (uniform-matrix program 'view-matrix (camera-matrix camera)))))))))
