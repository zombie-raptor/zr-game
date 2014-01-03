;;;; Sets up a very simple SDL2 and OpenGL program using shader files.

(in-package #:cl-foo)

;;; Return a shader from the given file.
(defun read-shader (shader-type filename)
  (let ((shader (gl:create-shader shader-type))
        (pathname (merge-pathnames (asdf:system-source-directory :cl-foo) filename)))
    (gl:shader-source shader (alexandria:read-file-into-string pathname))
    (gl:compile-shader shader)
    (let ((log (gl:get-shader-info-log shader)))
      (if (not (string= "" log))
          (error (concatenate 'string "Error in " filename "~%" log))))
    shader))

;;; Run a shader program with the given shaders.
(defun shader-program (shaders)
  (let ((program (gl:create-program)))
    (mapcar #'(lambda (shader) (gl:attach-shader program shader)) shaders)
    (gl:link-program program)
    (mapcar #'(lambda (shader) (gl:detach-shader program shader)) shaders)
    (mapcar #'gl:delete-shader shaders)
    (gl:use-program program)
    (let ((log (gl:get-program-info-log program)))
      (if (not (string= "" log))
          (error (concatenate 'string "Error in OpenGL shader program " "~%" log))))
    (gl:delete-program program)))

(defun main-loop (&key (width 1280) (height 720) (title "cl-foo"))
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title title :w width :h height :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
        ;; fixme: make sure the order is correct and everything is necessary
        (sdl2:gl-make-current win gl-context)
        (sdl2:hide-cursor)
        (gl:enable :depth-test)
        (glu:perspective 45.0 (/ width height) 0.1 100)
        (gl:clear-color 19/255 19/255 39/255 1.0)
        (gl:clear :color-buffer :depth-buffer)
        (shader-program (list (read-shader :vertex-shader "test.vert")
                              (read-shader :fragment-shader "test.frag")))

        ;; fixme: replace with array buffer for coordinates in modern OpenGL
        (gl:with-primitives :quads
          (gl:vertex -1.0 -1.0 -4.0)
          (gl:vertex 1.0 -1.0 -4.0)
          (gl:vertex 1.0 1.0 -4.0)
          (gl:vertex -1.0 1.0 -4.0))

        (gl:flush)
        (sdl2:gl-swap-window win)

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
