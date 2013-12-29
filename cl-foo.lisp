;;;; cl-foo.lisp

;(in-package #:cl-foo)

(require :sdl2)
(require :cl-opengl)
(require :cl-glu)

(defparameter *width* 1280)
(defparameter *height* 720)
(defparameter *ratio* (/ *width* *height*))

(defun main-loop ()
  (defun keydown-actions (scancode)
    (cond
      ((sdl2:scancode= scancode :scancode-w) "W")
      ((sdl2:scancode= scancode :scancode-s) "S")
      ((sdl2:scancode= scancode :scancode-a) "A")
      ((sdl2:scancode= scancode :scancode-d) "D")))

  (defun keyup-actions (scancode)
    (when (sdl2:scancode= scancode :scancode-escape)
      (sdl2:push-event :quit)))

  (defun init ()
    (gl:enable :cull-face :lighting :light0 :depth-test)
    (gl:matrix-mode :projection)
    (gl:load-identity)
;    (gl:frustum -2 2 -2 2 1 10)
    (gl:ortho 0 2 0 2 -2 10)
;    (glu:perspective 45.0 *ratio* 0.1 10)
    (gl:matrix-mode :modelview)
    (gl:load-identity)
    (gl:clear-color 19/255 19/255 39/255 1.0) ; #131327 background color
    (gl:clear :color-buffer :depth-buffer))

  (sdl2:with-init (:everything)
    (sdl2:with-window (win :w *width* :h *height* :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
        (sdl2:gl-make-current win gl-context)
        (sdl2:hide-cursor)
        (init)

        (sdl2:with-event-loop (:method :poll)
          (:keydown
           (:keysym keysym)
           (keydown-actions (sdl2:scancode-value keysym)))

          (:keyup
           (:keysym keysym)
           (keyup-actions (sdl2:scancode-value keysym)))

          ;; Used to draw a square.
          (:idle
           ()
           (gl:clear :color-buffer :depth-buffer)
           (gl:begin :quad-strip)
           (gl:material :front :ambient-and-diffuse #(1.0 0.0 0.0 1.0))
           (gl:vertex 0.0 1.0 2.0)
           (gl:vertex 1.0 1.0 2.0)
           (gl:vertex 1.0 0.0 2.0)
           (gl:vertex 0.0 0.0 2.0)
           (gl:end)
           (gl:flush)
           (sdl2:gl-swap-window win))

          (:quit () t))))))
