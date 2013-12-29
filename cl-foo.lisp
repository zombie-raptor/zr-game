;;;; cl-foo.lisp

;(in-package #:cl-foo)

(require :sdl2)

(defparameter *width* 1280)
(defparameter *height* 720)
(defparameter *scale* 10)
(defparameter *x* (* *width* (/ *scale* *height*)))
(defparameter *y* *scale*)

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
    (gl:enable :depth-test)
    (gl:viewport 0 0 *width* *height*)
    (gl:matrix-mode :projection)
    (gl:ortho (- *x*) *x* (- *y*) *y* (- *x*) *x*)
    (gl:matrix-mode :modelview)
    (gl:load-identity)
    (gl:clear-color 19/255 19/255 39/255 1.0) ; #131327 background color
    (gl:clear :color-buffer :depth-buffer))

  (sdl2:with-init (:everything)
    (sdl2:with-window (win :w *width* :h *height* :flags '(:shown :opengl))
      (sdl2:with-gl-context (gl-context win)
        (sdl2:gl-make-current win gl-context)
        (init)

        (sdl2:with-event-loop (:method :poll)
          (:keydown
           (:keysym keysym)
           (keydown-actions (sdl2:scancode-value keysym)))

          (:keyup
           (:keysym keysym)
           (keyup-actions (sdl2:scancode-value keysym)))

          ;; Draws a square.
          (:idle
           ()
           (gl:clear :color-buffer :depth-buffer)
           (gl:begin :quads)
           (gl:color 1.0 0.0 0.0)
           (gl:vertex -1.0 1.0 0.0)
           (gl:vertex 1.0 1.0 0.0)
           (gl:vertex 1.0 -1.0 0.0)
           (gl:vertex -1.0 -1.0 0.0)
           (gl:end)
           (gl:flush)
           (sdl2:gl-swap-window win))

          (:quit () t))))))
