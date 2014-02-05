;;;; This file contains more general stuff that doesn't (currently)
;;;; belong to another file.

(in-package #:cl-foo)

(defclass camera ()
  ((camera-eye
    :initarg :camera-eye
    :accessor camera-eye
    :initform (list 0.0 0.0 1.0))
   (camera-direction
    :initarg :camera-direction
    :accessor camera-direction
    :initform (list 0.0 0.0 0.0))
   (camera-up
    :initarg :camera-up
    :accessor camera-up
    :initform (list 0.0 1.0 0.0))))

(defgeneric move (object magnitude direction))

(defmethod move ((object camera) magnitude direction)
  (let ((i (case direction
             ((:x) 0)
             ((:y) 1)
             ((:z) 2))))
    (incf (elt (camera-eye object) i) magnitude)
    (incf (elt (camera-direction object) i) magnitude)))

(defun camera-matrix (camera)
  (look-at-matrix (camera-eye camera) (camera-direction camera) (camera-up camera)))

(defun move-camera (camera scancode)
  (cond ((sdl2:scancode= scancode :scancode-q) (move camera -0.1 :y))
        ((sdl2:scancode= scancode :scancode-e) (move camera 0.1 :y))
        ((sdl2:scancode= scancode :scancode-a) (move camera -0.1 :x))
        ((sdl2:scancode= scancode :scancode-d) (move camera 0.1 :x))
        ((sdl2:scancode= scancode :scancode-s) (move camera 0.1 :z))
        ((sdl2:scancode= scancode :scancode-w) (move camera -0.1 :z))))

(defmacro with-game-loop ((window keydown-actions) &body body)
  `(let ((keydown-scancodes nil))
     (sdl2:with-event-loop (:method :poll)
       (:keydown
        (:keysym keysym)
        (let ((scancode (sdl2:scancode-value keysym)))
          (setf keydown-scancodes (adjoin scancode keydown-scancodes))))

       (:keyup
        (:keysym keysym)
        (let ((scancode (sdl2:scancode-value keysym)))
          (if (member scancode keydown-scancodes)
              (setf keydown-scancodes (set-difference keydown-scancodes (list scancode))))
          (when (sdl2:scancode= scancode :scancode-escape)
            (sdl2:push-event :quit))))

       (:idle
        ()
        (gl:clear :color-buffer :depth-buffer)
        (if keydown-scancodes (map nil ,keydown-actions keydown-scancodes))
        (progn ,@body)
        (gl:flush)
        (sdl2:gl-swap-window ,window))

       (:quit
        ()
        t))))
