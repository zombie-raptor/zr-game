(in-package #:cl-foo)

(require :sdl2)
(require :cl-opengl)
(require :cl-glu)
(require :alexandria)

(defun read-shader (filename)
  (alexandria:read-file-into-string
   (merge-pathnames (asdf:system-source-directory :cl-foo) filename)))

(defun main-loop (&key (width 1280) (height 720))
  (sdl2:with-init (:everything)
    (sdl2:with-window (win :title "cl-foo" :w width :h height :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
        (sdl2:gl-make-current win gl-context)
        (sdl2:hide-cursor)

        (gl:enable :depth-test)

        (let ((vs (gl:create-shader :vertex-shader))
              (fs (gl:create-shader :fragment-shader))
              (program (gl:create-program)))

          (gl:shader-source vs (read-shader "test.vert"))
          (gl:shader-source fs (read-shader "test.frag"))
          (gl:compile-shader vs)
          (gl:compile-shader fs)
          (format t (gl:get-shader-info-log vs))
          (format t (gl:get-shader-info-log fs))
          (gl:attach-shader program vs)
          (gl:attach-shader program fs)
          (gl:link-program program)
          (glu:perspective 45.0 (/ width height) 0.1 100)
          (gl:clear-color 19/255 19/255 39/255 1.0) ; #131327 background color
          (gl:clear :color-buffer :depth-buffer)
          (gl:use-program program))

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
