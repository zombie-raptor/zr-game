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

(defun make-gl-array (type vect)
  (let ((array (gl:alloc-gl-array type (length vect))))
    (dotimes (i (length vect))
      (setf (gl:glaref array i) (aref vect i)))
    array))

;; Implementation of the gluPerspective matrix.
;; https://www.opengl.org/sdk/docs/man2/xhtml/gluPerspective.xml
(defun perspective (fovy aspect znear zfar)
  (let ((f (tan (* fovy (/ pi 360.0)))))
    (vector (/ aspect f) 0.0 0.0 0.0
            0.0 (/ f) 0.0 0.0
            0.0 0.0 (/ (+ zfar znear) (- znear zfar)) (/ (* 2.0 zfar znear) (- znear zfar))
            0.0 0.0 -1.0 0.0)))

;; Calculates the magnitude of a list representing a mathematical
;; vector.
(defun magnitude (l)
  (sqrt (reduce '+ (mapcar #'(lambda (x) (expt x 2)) l))))

;; Returns a unit vector of a list representing a mathematical vector.
(defun normalize (l)
  (mapcar #'(lambda (x) (/ x (magnitude l))) l))

;; Using one of the definitions from Wikipedia. I hope this works.
(defun cross-product (l1 l2)
  (list (- (* (nth 1 l1) (nth 2 l2)) (nth 2 l1) (nth 1 l2))
        (- (* (nth 2 l1) (nth 0 l2)) (nth 0 l1) (nth 2 l2))
        (- (* (nth 0 l1) (nth 1 l2)) (nth 1 l1) (nth 0 l2))))

;; Very naive reimplementation of the gluLookAt matrix.
;; https://www.opengl.org/sdk/docs/man2/xhtml/gluLookAt.xml
(defun look-at (eye center up)
  (let* ((f (normalize (mapcar #'- center eye)))
         (s (cross-product f (normalize up)))
         (u (cross-product (normalize s) f)))
    (coerce (append s '(0)
                    u '(0)
                    (mapcar #'- f) '(0)
                    '(0 0 0 1))
            'vector)))

(defun main-loop (&key (width 1280) (height 720) (title "cl-foo"))
  (sdl2:with-init (:everything)
    (sdl2:with-window (window :title title :w width :h height :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context window)
        (with-buffers (buffers :count 1)
          (with-shaders (shaders program :shader-list '("test.vert" "test.frag") :dir (asdf:system-source-directory :cl-foo))
            (sdl2:gl-make-current window gl-context)
            (sdl2:hide-cursor)
            (gl:enable :depth-test)
            (gl:clear-color 0.0 0 0 1.0)
            (gl:clear :color-buffer :depth-buffer)
            (let ((coords (make-gl-array :float #(-0.5  0.5 -1.0
                                                   0.5  0.5 -1.0
                                                   0.5 -0.5 -1.0
                                                  -0.5 -0.5 -1.0
                                                  -0.5  0.5 -1.0))))
              (gl:use-program program)
              (gl:uniform-matrix (gl:get-uniform-location program "projection_matrix") 4
                                 (vector (perspective 45.0 (/ width height) 0.1 100.0)))
              (gl:uniform-matrix (gl:get-uniform-location program "view_matrix") 4
                                 (vector (look-at '(2.0 2.0 -1.0) '(0.0 0.0 -3.0) '(0.0 1.0 0.0))))
              (gl:bind-buffer :array-buffer (nth 0 buffers))
              (gl:buffer-data :array-buffer :static-draw coords)
              (gl:vertex-attrib-pointer 0 3 :float nil 0 0)
              (gl:enable-vertex-attrib-array 0)
              (gl:draw-arrays :triangles 0 3)
              (gl:draw-arrays :triangles 2 3)
              (gl:disable-vertex-attrib-array 0)
              (gl:free-gl-array coords))
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
