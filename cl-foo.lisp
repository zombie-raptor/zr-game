;;;; This file might do something some day.

(in-package #:cl-foo)

;; Currently assumes .vert or .frag are the only possible extensions
;; for shader files, and reads them in as a string.
(defun read-shader (filename directory)
  (let ((shader (gl:create-shader (if (string= ".vert" (subseq filename (- (length filename) 5)))
                                      :vertex-shader
                                      :fragment-shader))))
    (gl:shader-source shader (alexandria:read-file-into-string (merge-pathnames directory filename)))
    (gl:compile-shader shader)
    (if (not (gl:get-shader shader :compile-status))
        (error (concatenate 'string "Error in " filename "~%" (gl:get-shader-info-log shader))))
    shader))

(defun shader-program (shaders)
  (let ((program (gl:create-program)))
    (mapcar #'(lambda (shader) (gl:attach-shader program shader)) shaders)
    (gl:link-program program)
    (if (not (gl:get-program program :link-status))
        (error (concatenate 'string "Error in shader program~%" (gl:get-program-info-log program))))
    (mapcar #'(lambda (shader) (gl:detach-shader program shader)) shaders)
    program))

(defmacro with-shaders ((shaders program &key shader-list dir) &body body)
  `(let* ((,shaders (mapcar #'(lambda (x) (read-shader x ,dir)) ,shader-list))
          (,program (shader-program ,shaders)))
     (unwind-protect
          (progn ,@body)
       (progn
         (mapcar #'gl:delete-shader ,shaders)
         (gl:delete-program ,program)))))

(defmacro with-buffers ((buffers &key (count 1)) &body body)
  `(let ((,buffers (gl:gen-buffers ,count)))
     (unwind-protect
          (progn ,@body)
       (gl:delete-buffers ,buffers))))

(defun gl-array (buffer-type buffer array-type vect)
  (let ((array (gl:alloc-gl-array array-type (length vect))))
    (dotimes (i (length vect))
      (setf (gl:glaref array i) (aref vect i)))
    (gl:bind-buffer buffer-type buffer)
    (gl:buffer-data buffer-type :static-draw array)
    (gl:free-gl-array array)
    (gl:bind-buffer buffer-type 0)
    t))

;; Each of the 6 faces in a cube is a pair of two triangles who share
;; the beginning and end points. Each loop here is a face.
(defun get-cube-elements ()
  (let ((v nil))
    (dotimes (i 6)
      (let ((x (* i 4)))
        (setf v (concatenate 'vector v (vector x (+ x 1) (+ x 2) (+ x 2) (+ x 3) x)))))
    v))

;; fixme: Can I replace this with an algorithm to generate these
;; coordinates on startup and make the cube's geometry more obvious?
;; This could then generalize into other geometric shapes perhaps.
(defun get-cube-points ()
  #(-1.0 -1.0 1.0
    1.0 -1.0 1.0
    1.0 1.0 1.0
    -1.0 1.0 1.0

    -1.0 1.0 1.0
    1.0 1.0 1.0
    1.0 1.0 -1.0
    -1.0 1.0 -1.0

    1.0 -1.0 -1.0
    -1.0 -1.0 -1.0
    -1.0 1.0 -1.0
    1.0 1.0 -1.0

    -1.0 -1.0 -1.0
    1.0 -1.0 -1.0
    1.0 -1.0 1.0
    -1.0 -1.0 1.0

    -1.0 -1.0 -1.0
    -1.0 -1.0 1.0
    -1.0 1.0 1.0
    -1.0 1.0 -1.0

    1.0 -1.0 1.0
    1.0 -1.0 -1.0
    1.0 1.0 -1.0
    1.0 1.0 1.0))

(defun main-loop (&key (width 1280) (height 720) (title "cl-foo"))
  (sdl2:with-init (:everything)
    (sdl2:with-window (window :title title :w width :h height :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context window)
        (sdl2:gl-make-current window gl-context)
        (sdl2:hide-cursor)
        (gl:enable :depth-test)
        (with-buffers (buffers :count 2)
          (with-shaders (shaders program :shader-list '("test.vert" "test.frag") :dir (asdf:system-source-directory :cl-foo))
            (gl:use-program program)
            (gl:uniform-matrix (gl:get-uniform-location program "projection_matrix") 4
                               (vector (perspective-matrix 45.0 (/ width height) 0.1 100.0)))
            (gl:uniform-matrix (gl:get-uniform-location program "view_matrix") 4
                               (vector (look-at-matrix #(0.0 0.0 1.0) #(0.0 0.0 0.0) #(0.0 1.0 0.0))))
            (gl-array :array-buffer (elt buffers 0) :float (get-cube-points))
            (gl-array :element-array-buffer (elt buffers 1) :unsigned-short (get-cube-elements))
            (gl:bind-buffer :array-buffer (elt buffers 0))
            (gl:bind-buffer :element-array-buffer (elt buffers 1))
            (gl:enable-vertex-attrib-array 0)
            (gl:vertex-attrib-pointer 0 3 :float nil 0 0)
            (gl:bind-vertex-array 0)
            (gl:clear-color 0 0 0 1)
            (gl:clear :color-buffer :depth-buffer)
            (dotimes (i 4)
              (let ((x (+ -3.0 (* i 2)))
                    (y (+ -3.0 (* i 2))))
              (gl:uniform-matrix (gl:get-uniform-location program "translation_matrix") 4
                                 (vector (translation-matrix x y -10.0))))
              (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short) :count 36))
            (gl:disable-vertex-attrib-array 0)
            (gl:use-program 0)
            (gl:flush)
            (sdl2:gl-swap-window window)

            (sdl2:with-event-loop (:method :poll)
              (:keydown
               (:keysym keysym)
               ;; fixme: make the keys do something again
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

              (:quit () t))))))))
