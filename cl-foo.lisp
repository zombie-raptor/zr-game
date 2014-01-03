(in-package #:cl-foo)

(require :sdl2)
(require :cl-opengl)
(require :cl-glu)
(require :alexandria)

(defun read-shader (filename)
  (alexandria:read-file-into-string
   (merge-pathnames (asdf:system-source-directory :cl-foo) filename)))

(defun use-shader (shader-type filename program)
  (let ((shader (gl:create-shader shader-type)))
    (gl:shader-source shader (read-shader filename))
    (gl:compile-shader shader)
    (gl:attach-shader program shader)
    (gl:link-program program)
    (gl:detach-shader program shader)
    (gl:delete-shader shader)))

(defun main-loop (&key (width 1280) (height 720))
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "cl-foo" :w width :h height :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
        (sdl2:gl-make-current win gl-context)

        (let ((program (gl:create-program)))
          (sdl2:hide-cursor)
          (gl:enable :depth-test)
          (use-shader :vertex-shader "test.vert" program)
          (use-shader :fragment-shader "test.frag" program)
          (glu:perspective 45.0 (/ width height) 0.1 100)
          (gl:clear-color 19/255 19/255 39/255 1.0) ; #131327 background color
          (gl:clear :color-buffer :depth-buffer)
          (gl:use-program program)
          (gl:delete-program program)

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

            (:quit () t)))))))
