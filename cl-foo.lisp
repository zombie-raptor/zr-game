;;;; Sets up a very simple SDL2 and OpenGL program using shader files.

(in-package #:cl-foo)

;;; Return a shader from the given file.
(defun read-shader (shader-type filename)
  (let ((shader (gl:create-shader shader-type))
        (pathname (merge-pathnames (asdf:system-source-directory :cl-foo) filename)))
    (gl:shader-source shader (alexandria:read-file-into-string pathname))
    (gl:compile-shader shader)
    (if (not (gl:get-shader shader :compile-status))
        (error (concatenate 'string "Error in " filename "~%" (gl:get-shader-info-log shader))))
    shader))

;;; Run a shader program with the given shaders.
(defun shader-program (shaders)
  (let ((program (gl:create-program)))
    (mapcar #'(lambda (shader) (gl:attach-shader program shader)) shaders)
    (gl:link-program program)
    (if (not (gl:get-program program :link-status))
        (error (concatenate 'string "Error in shader program~%" (gl:get-program-info-log program))))
    (mapcar #'(lambda (shader) (gl:detach-shader program shader)) shaders)
    (gl:use-program program)
    (gl:delete-program program)
    t))

(defun make-gl-array (vect)
  (let ((array (gl:alloc-gl-array :float (length vect))))
    (dotimes (i (length vect))
      (setf (gl:glaref array i) (aref vect i)))
    array))

(defmacro with-gl-buffers ((buffers count) &body body)
  `(let ((,buffers (gl:gen-buffers ,count)))
     (unwind-protect
          (progn ,@body)
       (gl:delete-buffers buffers))))

(defun main-loop (&key (width 1280) (height 720) (title "cl-foo"))
  (sdl2:with-init (:everything)
    (sdl2:with-window (window :title title :w width :h height :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context window)
        (sdl2:gl-make-current window gl-context)
        (sdl2:hide-cursor)
        (gl:enable :depth-test)
        (gl:clear-color 19/255 19/255 39/255 1.0)
        (gl:clear :color-buffer)

        (with-gl-buffers (buffers 1)
          (let ((shaders (list (read-shader :vertex-shader "test.vert")
                               (read-shader :fragment-shader "test.frag")))
                (coords (make-gl-array #(0.0 1.0 -1.0
                                         1.0 1.0 -1.0
                                         1.0 0.0 -1.0
                                         0.0 0.0 -1.0
                                         0.0 1.0 -1.0))))

            (gl:bind-buffer :array-buffer (nth 0 buffers))
            (gl:buffer-data :array-buffer :static-draw coords)
            (gl:vertex-attrib-pointer 0 3 :float nil 0 0)
            (gl:enable-vertex-attrib-array 0)
            (shader-program shaders)
            (gl:draw-arrays :triangles 0 3)
            (gl:draw-arrays :triangles 2 3)
            (mapcar #'gl:delete-shader shaders)
            (gl:free-gl-array coords)))

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

          (:quit () t))))))
