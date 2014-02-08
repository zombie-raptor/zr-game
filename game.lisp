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
