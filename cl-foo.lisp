;;;; This file is currently being used to try to get some very basic
;;;; OpenGL things rendered. As I learn more about what OpenGL
;;;; expects, useful functions and macros will be spun off into their
;;;; own files.

;;;; What will this evolve into? I have no clue.

(in-package #:cl-foo)

(defparameter *shaders*
  (list (make-glsl-shader '((:defvar position vec3 :storage in :location 0)
                            (:defvar offset vec3 :storage uniform)
                            (:defvar view-matrix mat4 :storage uniform)
                            (:defvar projection-matrix mat4 :storage uniform)
                            (:defun main void ()
                             (:setf gl-position (:* projection-matrix
                                                    view-matrix
                                                    (:vec4 (:+ position offset) 1.0))))))
        (make-glsl-shader '((:defvar out-color vec4 :storage out)
                            (:defun main void ()
                             (:setf out-color (:vec4 0.5 0.5 1.0 1.0)))))))

;;; Experimenting with look-at code for the camera. 0 is x, 1 is y,
;;; 2 is z.
(defun move-camera (camera scancode)
  ;; Moves the direction the camera is pointing?
  (if (sdl2:scancode= scancode :scancode-w) (incf (elt (camera-direction camera) 1) -0.01))
  (if (sdl2:scancode= scancode :scancode-s) (incf (elt (camera-direction camera) 1) 0.01))
  (if (sdl2:scancode= scancode :scancode-a) (incf (elt (camera-direction camera) 0) -0.01))
  (if (sdl2:scancode= scancode :scancode-d) (incf (elt (camera-direction camera) 0) 0.01))
  ;; Moves the location of the camera?
  (if (sdl2:scancode= scancode :scancode-t) (incf (elt (camera-eye camera) 1) -0.01))
  (if (sdl2:scancode= scancode :scancode-g) (incf (elt (camera-eye camera) 1) 0.01))
  (if (sdl2:scancode= scancode :scancode-f) (incf (elt (camera-eye camera) 0) -0.01))
  (if (sdl2:scancode= scancode :scancode-h) (incf (elt (camera-eye camera) 0) 0.01))
  (if (sdl2:scancode= scancode :scancode-r) (incf (elt (camera-eye camera) 2) -0.01))
  (if (sdl2:scancode= scancode :scancode-y) (incf (elt (camera-eye camera) 2) 0.01)))

(defmacro with-game-loop ((window) &body body)
  `(let ((keydown-scancodes nil))
     (sdl2:with-event-loop (:method :poll)
       (:keydown
        (:keysym keysym)
        (let ((scancode (sdl2:scancode-value keysym)))
          (setf keydown-scancodes (adjoin scancode keydown-scancodes))))

       (:keyup
        (:keysym keysym)
        (let ((scancode (sdl2:scancode-value keysym)))
          (if (member scancode keydown-scancodes)
              (setf keydown-scancodes (set-difference keydown-scancodes (list scancode))))
          (when (sdl2:scancode= scancode :scancode-escape)
            (sdl2:push-event :quit))))

       (:idle
        ()
        (progn ,@body)
        (gl:flush)
        (sdl2:gl-swap-window ,window))

       (:quit
        ()
        t))))

(defun main-loop (&key (width 1280) (height 720) (title "OpenGL Rendering Test"))
  (with-sdl2 (window :title title :width width :height height)
    (with-buffers (buffers :count 2)
      (with-shaders (shaders program :shader-list *shaders* :shader-type-list '(:vertex-shader :fragment-shader))
        (let ((camera-test (make-instance 'camera))
              (array-buffer (elt buffers 0))
              (element-array-buffer (elt buffers 1))
              (cube-points (get-cube-group-points 8 :offset #(0.0 -4.0 -10.0)))
              (cube-elements (get-cube-elements 8)))
          (gl:use-program program)
          (uniform-matrix program 'projection-matrix (perspective-matrix 45.0 (/ width height) 0.1 100.0))
          (gl-array :array-buffer array-buffer :float cube-points)
          (gl-array :element-array-buffer element-array-buffer :unsigned-short cube-elements)
          (gl:use-program 0)
          (with-game-loop (window)
            ;; FIXME: Relies on a list defined within with-game-loop.
            (if keydown-scancodes (map nil #'(lambda (scancode) (move-camera camera-test scancode)) keydown-scancodes))
            (with-vertex-attrib-array (program array-buffer element-array-buffer 0 3 :float)
              (gl:clear :color-buffer :depth-buffer)
              (uniform-vector program 'offset #(1.0 -2.0 -10.0))
              (uniform-matrix program 'view-matrix (look-at-matrix (camera-eye camera-test)
                                                                   (camera-direction camera-test)
                                                                   (camera-up camera-test)))
              (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short) :count (length cube-elements)))))))))
