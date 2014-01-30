;;;; This file is currently being used to try to get some very basic
;;;; OpenGL things rendered. As I learn more about what OpenGL
;;;; expects, useful functions and macros will be spun off into their
;;;; own files.

;;;; What will this evolve into? I have no clue.

(in-package #:cl-foo)

(defparameter *shaders*
  (list (make-glsl-shader '((:defvar position vec3 :storage in :location 0)
                            (:defvar projection-matrix mat4 :storage uniform)
                            (:defvar view-matrix mat4 :storage uniform)
                            (:defvar translation-matrix mat4 :storage uniform)
                            (:defun main void ()
                             (:setf gl-position (:* projection-matrix
                                                    view-matrix
                                                    translation-matrix
                                                    (:vec4 position 1.0))))))
        (make-glsl-shader '((:defvar out-color vec4 :storage out)
                            (:defun main void ()
                             (:setf out-color (:vec4 0.5 0.5 1.0 1.0)))))))

(defun main-loop (&key (width 1280) (height 720) (title "OpenGL Rendering Test"))
  (with-sdl2 (window :title title :width width :height height)
    (with-buffers (buffers :count 2)
      (with-shaders (shaders program :shader-list *shaders* :shader-type-list '(:vertex-shader :fragment-shader))
        (gl:use-program program)
        (uniform-matrix program 'projection-matrix (perspective-matrix 45.0 (/ width height) 0.1 100.0))
        (uniform-matrix program 'view-matrix (look-at-matrix #(0.0 0.0 1.0) #(0.0 0.0 0.0) #(0.0 1.0 0.0)))
        (gl-array :array-buffer (elt buffers 0) :float (get-cube-points 1.0))
        (gl-array :element-array-buffer (elt buffers 1) :unsigned-short (get-cube-elements))
        (with-vertex-attrib-array ((elt buffers 0) (elt buffers 1) 0 3 :float)
          (gl:bind-vertex-array 0)
          (gl:clear-color 0 0 0 1)
          (gl:clear :color-buffer :depth-buffer)
          (dotimes (i 4)
            (let ((x (+ -3.0 (* i 2)))
                  (y (+ -3.0 (* i 2))))
              (uniform-matrix program 'translation-matrix (translation-matrix x y -10.0)))
            (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short) :count 36)))
        (gl:use-program 0)
        (gl:flush)
        (sdl2:gl-swap-window window)

        (sdl2:with-event-loop (:method :poll)
          ;; FIXME: Currently does nothing. It's kept here as a
          ;; reminder of the syntax.
          (:keydown
           (:keysym keysym)
           (let ((scancode (sdl2:scancode-value keysym)))
             (cond
               ((sdl2:scancode= scancode :scancode-w) "W")
               ((sdl2:scancode= scancode :scancode-s) "S")
               ((sdl2:scancode= scancode :scancode-a) "A")
               ((sdl2:scancode= scancode :scancode-d) "D"))))

          (:keyup
           (:keysym keysym)
           (let ((scancode (sdl2:scancode-value keysym)))
             (when (sdl2:scancode= scancode :scancode-escape)
               (sdl2:push-event :quit))))

          (:idle ())

          (:quit () t))))))
