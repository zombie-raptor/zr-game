;;;; cl-foo.lisp

;(in-package #:cl-foo)

(require :sdl2)

(defun basic-test ()
  "Working code to start with. Based on cl-sdl2's demos."

  (defun keydown-actions (scancode sym mod-value)
    (cond
      ((sdl2:scancode= scancode :scancode-w) (format t "~a~%" "WALK"))
      ((sdl2:scancode= scancode :scancode-s) (sdl2:show-cursor))
      ((sdl2:scancode= scancode :scancode-h) (sdl2:hide-cursor)))

    (format t "Key sym: ~a, code: ~a, mod: ~a~%"
            sym
            scancode
            mod-value))

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
           (keydown-actions (sdl2:scancode-value keysym) (sdl2:sym-value keysym) (sdl2:mod-value keysym)))

          (:keyup
           (:keysym keysym)
           (keyup-actions (sdl2:scancode-value keysym)))

          (:mousemotion
           (:x x :y y :xrel xrel :yrel yrel :state state)
           (format t "Mouse motion abs(rel): ~a (~a), ~a (~a)~%Mouse state: ~a~%"
                   x xrel y yrel state))

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
