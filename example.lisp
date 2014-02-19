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

;;; Currently, configuration is done only through the arguments to
;;; this function. Eventually a menu and configuration files will be
;;; included.
(defun example (&key (width 1280) (height 720) (mouse-sensitivity 10) (title "OpenGL Rendering Test") (fullscreen nil))
  (sdl2:with-everything (:window (win :title title :w width :h height :fullscreen fullscreen) :gl gl)
    (with-buffers (buffers :count 2)
      (with-shaders (shaders program *shaders*)
        (sdl2:hide-cursor)
        (setup-gl)
        (let* ((main-camera (make-instance 'camera))
               (cube-group (get-cube-group 2 2 100 :offset #(0.0 -4.0 -10.0 0.0)))
               (cubes (make-instance 'vao
                                     :program program
                                     :array (elt cube-group 1)
                                     :element-array (elt cube-group 0)
                                     :array-buffer (elt buffers 0)
                                     :element-array-buffer (elt buffers 1)
                                     :in-variable 'position)))
          (with-shader-program (program)
            (uniform-matrix program 'projection-matrix (perspective-matrix 45.0 (/ width height) 0.1 200.0)))
          (with-game-loop (win keydown-scancodes mouse-motion)
            (keyboard-move main-camera keydown-scancodes :speed 0.1)
            (mouse-move main-camera (elt mouse-motion 0) (elt mouse-motion 1) width height
                               :sensitivity mouse-sensitivity
                               :capture-window win)
            (with-shader-program (program)
              (uniform-matrix program 'view-matrix (get-matrix main-camera)))
            (draw cubes)))))))
