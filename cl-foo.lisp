(in-package #:cl-foo)

(require :sdl2)
(require :cl-opengl)
(require :cl-glu)
(require :alexandria)

(defun get-shader (shader-type filename)
  (let ((shader (gl:create-shader shader-type))
        (pathname (merge-pathnames (asdf:system-source-directory :cl-foo) filename)))
    (gl:shader-source shader (alexandria:read-file-into-string pathname))
    (gl:compile-shader shader)
    shader))

(defun shader-program (shaders)
  (let ((program (gl:create-program)))
    (mapcar #'(lambda (shader) (gl:attach-shader program shader)) shaders)
    (gl:link-program program)
    (mapcar #'(lambda (shader) (gl:detach-shader program shader)) shaders)
    (mapcar #'gl:delete-shader shaders)
    (gl:use-program program)
    (gl:delete-program program)))

(defun main-loop (&key (width 1280) (height 720))
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "cl-foo" :w width :h height :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
        (sdl2:gl-make-current win gl-context)
        (sdl2:hide-cursor)
        (gl:enable :depth-test)
        (glu:perspective 45.0 (/ width height) 0.1 100)
        (gl:clear-color 19/255 19/255 39/255 1.0) ; #131327 background color
        (gl:clear :color-buffer :depth-buffer)
        (shader-program (list (get-shader :vertex-shader "test.vert")
                              (get-shader :fragment-shader "test.frag")))

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

          (:idle
           ())

          (:quit () t))))))
