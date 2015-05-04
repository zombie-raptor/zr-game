;;;; This file contains more general stuff that doesn't (currently)
;;;; belong to another file.

(in-package #:zr-game)

;;;; Cameras

(defgeneric get-matrix (object))
(defgeneric move (object magnitude direction))
(defgeneric rotate-object (object x-z-angle y-angle))
(defgeneric keyboard-move (object scancodes &key speed))
(defgeneric mouse-move (object x y w h &key capture-window sensitivity))

(defclass camera ()
  ((x-z-angle
    :initarg :x-z-angle
    :accessor camera-x-z-angle
    :initform -90.0)
   (y-z-angle
    :initarg :y-z-angle
    :accessor camera-y-z-angle
    :initform 0.0)
   (world-offset
    :initarg :world-offset
    :accessor world-offset
    :initform (make-array 3 :initial-element 0.0))))

(defmethod get-matrix ((camera camera))
  (sb-cga:matrix* (sb-cga:rotate* (coerce (* (camera-y-z-angle camera) pi (/ 180)) 'single-float) 0.0 0.0)
                  (sb-cga:rotate* 0.0 (coerce (* (camera-x-z-angle camera) pi (/ 180)) 'single-float) 0.0)
                  (sb-cga:translate (sb-cga:vec (elt (world-offset camera) 0)
                                                (elt (world-offset camera) 1)
                                                (elt (world-offset camera) 2)))))

(defmethod move ((camera camera) magnitude direction)
  (let ((i (case direction
             ((:x) 0)
             ((:y) 1)
             ((:z) 2))))
    (incf (elt (world-offset camera) i) (- magnitude))))

(defmethod rotate-object ((camera camera) x-z-angle y-z-angle)
  (incf (camera-x-z-angle camera) x-z-angle)
  (cond ((>= (+ (camera-y-z-angle camera) y-z-angle) 90) (setf (camera-y-z-angle camera) 90))
        ((<= (+ (camera-y-z-angle camera) y-z-angle) -90) (setf (camera-y-z-angle camera) -90))
        (t (incf (camera-y-z-angle camera) y-z-angle))))

;;; FIXME: This assumes x and z are facing 0 degrees. It needs to
;;; rotate with the x-z-angle. i.e. I broke it again. The work should
;;; be done in the matrices.
(defmethod keyboard-move ((camera camera) scancodes &key (speed 0.1))
  (map nil
       #'(lambda (scancode)
           (scancode-case (scancode)
                          (:scancode-q (move camera (- speed) :y))
                          (:scancode-e (move camera speed :y))
                          (:scancode-s (move camera (- speed) :x))
                          (:scancode-w (move camera speed :x))
                          (:scancode-a (move camera (- speed) :z))
                          (:scancode-d (move camera speed :z))))
       scancodes))

(defmethod mouse-move ((camera camera) x y w h &key (capture-window nil) (sensitivity 10))
  (rotate-object camera (* 5 sensitivity x (/ w)) (* 3 sensitivity y (/ h)))
  (if capture-window (sdl2:warp-mouse-in-window capture-window (/ h 2) (/ w 2))))
