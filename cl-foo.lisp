;;;; cl-foo.lisp

;(in-package #:cl-foo)

(require :sdl2)

(defun main-loop ()
  (defun keydown-actions (scancode)
    (format t "~a~%" (cond
                       ((sdl2:scancode= scancode :scancode-w) "W")
                       ((sdl2:scancode= scancode :scancode-s) "S")
                       ((sdl2:scancode= scancode :scancode-a) "A")
                       ((sdl2:scancode= scancode :scancode-d) "D")
                       (t "foo"))))

  (defun keyup-actions (scancode)
    (when (sdl2:scancode= scancode :scancode-escape)
      (sdl2:push-event :quit)))

  (sdl2:with-init (:everything)
    (sdl2:with-window (win :w 1280 :h 720 :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
        (sdl2:gl-make-current win gl-context)
        (gl:viewport 0 0 1280 720)
        (gl:matrix-mode :projection)
        (gl:ortho -2 2 -2 2 -2 2)
        (gl:matrix-mode :modelview)
        (gl:load-identity)
        (gl:clear-color 19/255 19/255 39/255 1.0) ; #131327 background color
        (gl:clear :color-buffer)

        (sdl2:with-event-loop (:method :poll)
          (:keydown
           (:keysym keysym)
           (keydown-actions (sdl2:scancode-value keysym)))

          (:keyup
           (:keysym keysym)
           (keyup-actions (sdl2:scancode-value keysym)))

          (:idle
           ()
           (gl:clear :color-buffer)
           (gl:begin :triangles)
           (gl:color 1.0 0.0 0.0)
           (gl:vertex 0.0 1.0)
           (gl:vertex -1.0 -1.0)
           (gl:vertex 1.0 -1.0)
           (gl:end)
           (gl:flush)
           (sdl2:gl-swap-window win))

          (:quit () t))))))
