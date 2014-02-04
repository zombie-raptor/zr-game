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

(defun move-camera (camera scancode)
  (cond ((sdl2:scancode= scancode :scancode-q) (move camera -0.1 :y))
        ((sdl2:scancode= scancode :scancode-e) (move camera 0.1 :y))
        ((sdl2:scancode= scancode :scancode-a) (move camera -0.1 :x))
        ((sdl2:scancode= scancode :scancode-d) (move camera 0.1 :x))
        ((sdl2:scancode= scancode :scancode-s) (move camera 0.1 :z))
        ((sdl2:scancode= scancode :scancode-w) (move camera -0.1 :z))))

(defmacro with-game-loop ((window keydown-actions) &body body)
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
        (gl:clear :color-buffer :depth-buffer)
        (if keydown-scancodes (map nil ,keydown-actions keydown-scancodes))
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
          (with-shader-program (program)
            (uniform-matrix program 'projection-matrix (perspective-matrix 45.0 (/ width height) 0.1 100.0))
            (gl-array :array-buffer array-buffer :float cube-points)
            (gl-array :element-array-buffer element-array-buffer :unsigned-short cube-elements))
          (with-game-loop (window #'(lambda (scancode) (move-camera camera-test scancode)))
            (with-vertex-attrib-array (program array-buffer element-array-buffer 0 3 :float)
              (uniform-vector program 'offset #(1.0 -2.0 -10.0))
              (uniform-matrix program 'view-matrix (look-at-matrix (camera-eye camera-test)
                                                                   (camera-direction camera-test)
                                                                   (camera-up camera-test)))
              (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short) :count (length cube-elements)))))))))
