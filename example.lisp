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
                                 (:defvar offset :vec4 :storage uniform)
                                 (:defvar view-matrix :mat4 :storage uniform)
                                 (:defvar projection-matrix :mat4 :storage uniform)
                                 (:defun main :void ()
                                         (:setf gl-position (:* projection-matrix
                                                                view-matrix
                                                                (:+ position offset))))))
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
  (with-sdl2 (window :title title :width width :height height :fullscreen fullscreen)
    (with-buffers (buffers :count 2)
      (with-shaders (shaders program *shaders*)
        (sdl2:hide-cursor)
        (setup-gl)
        (let* ((main-camera (make-instance 'camera
                                           :x-z-angle -90.0
                                           :y-angle 0.0
                                           :world-offset #(0.0 0.0 0.0 1.0)))
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
          (with-game-loop (window keydown-scancodes mouse-motion)
            (if keydown-scancodes (map nil
                                       #'(lambda (scancode) (move-camera main-camera scancode))
                                       keydown-scancodes))
            (rotate-object main-camera
                           (* 5 mouse-sensitivity (elt mouse-motion 0) (/ width))
                           (* -3 mouse-sensitivity (elt mouse-motion 1) (/ height)))
            (sdl2:warp-mouse-in-window window (/ height 2) (/ width 2))
            (with-shader-program (program)
              (uniform-vector program 'offset (world-offset main-camera))
              (uniform-matrix program 'view-matrix (get-matrix main-camera)))
            (draw cubes)))))))
