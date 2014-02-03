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

(defun main-loop (&key (width 1280) (height 720) (title "OpenGL Rendering Test"))
  (with-sdl2 (window :title title :width width :height height)
    (with-buffers (buffers :count 2)
      (with-shaders (shaders program :shader-list *shaders* :shader-type-list '(:vertex-shader :fragment-shader))
        (let ((camera-test (make-instance 'camera))
              (array-buffer (elt buffers 0))
              (element-array-buffer (elt buffers 1))
              (cube-points (concatenate 'vector
                                        (get-cube-points :offset #(0.0 9.0 -10.0))
                                        (get-cube-points :offset #(0.0 7.0 -10.0))))
              (cube-elements (get-cube-elements 2)))
          (gl:use-program program)
          (uniform-matrix program 'projection-matrix (perspective-matrix 45.0 (/ width height) 0.1 100.0))
          (gl-array :array-buffer array-buffer :float cube-points)
          (gl-array :element-array-buffer element-array-buffer :unsigned-short cube-elements)
          (gl:clear-color 0 0 0 1)
          (sdl2:with-event-loop (:method :poll)
            (:keydown
             (:keysym keysym)
             (let ((scancode (sdl2:scancode-value keysym)))
               (if (sdl2:scancode= scancode :scancode-w) (incf (elt (camera-direction camera-test) 1) -0.01))
               (if (sdl2:scancode= scancode :scancode-s) (incf (elt (camera-direction camera-test) 1) 0.01))
               (if (sdl2:scancode= scancode :scancode-a) (incf (elt (camera-direction camera-test) 0) -0.01))
               (if (sdl2:scancode= scancode :scancode-d) (incf (elt (camera-direction camera-test) 0) 0.01))
               (if (sdl2:scancode= scancode :scancode-q) (incf (elt (camera-direction camera-test) 2) -0.01))
               (if (sdl2:scancode= scancode :scancode-e) (incf (elt (camera-direction camera-test) 2) 0.01))
               (if (sdl2:scancode= scancode :scancode-t) (incf (elt (camera-eye camera-test) 1) -0.01))
               (if (sdl2:scancode= scancode :scancode-g) (incf (elt (camera-eye camera-test) 1) 0.01))
               (if (sdl2:scancode= scancode :scancode-f) (incf (elt (camera-eye camera-test) 0) -0.01))
               (if (sdl2:scancode= scancode :scancode-h) (incf (elt (camera-eye camera-test) 0) 0.01))
               (if (sdl2:scancode= scancode :scancode-r) (incf (elt (camera-eye camera-test) 2) -0.01))
               (if (sdl2:scancode= scancode :scancode-y) (incf (elt (camera-eye camera-test) 2) 0.01))))

            (:keyup
             (:keysym keysym)
             (let ((scancode (sdl2:scancode-value keysym)))
               (when (sdl2:scancode= scancode :scancode-escape)
                 (sdl2:push-event :quit))))

            ;; (:mousemotion
            ;;  (:xrel xrel :yrel yrel)
            ;;  (setf (camera-eye camera-test) (vector (- (elt (camera-eye camera-test) 0) (/ xrel width 1/32)) (- (elt (camera-eye camera-test) 1) (/ yrel height 0.5)) 1.0)))

            (:idle
             ()
             (sleep 1/30)
             (with-vertex-attrib-array (program array-buffer element-array-buffer 0 3 :float)
               (gl:clear :color-buffer :depth-buffer)
               (uniform-vector program 'offset #(1.0 -2.0 -10.0))
               (uniform-matrix program 'view-matrix (look-at-matrix (camera-eye camera-test)
                                                                    (camera-direction camera-test)
                                                                    (camera-up camera-test)))
               (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short) :count (length cube-elements)))
             (gl:flush)
             (sdl2:gl-swap-window window))

            (:quit () t)))))))
