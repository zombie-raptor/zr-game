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
   (y-angle
    :initarg :y-angle
    :accessor camera-y-angle
    :initform 0.0)
   (world-offset
    :initarg :world-offset
    :accessor world-offset
    :initform (make-array 3 :initial-element 0.0))))

(defmethod get-matrix ((camera camera))
  (sb-cga:matrix* (camera-matrix (vector 0.0 (coerce (sin (* (camera-y-angle camera) pi (/ 180))) 'single-float) -1.0) #(0.0 1.0 0.0))
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

(defmethod rotate-object ((camera camera) x-z-angle y-angle)
  (incf (camera-x-z-angle camera) x-z-angle)
  (cond ((>= (+ (camera-y-angle camera) y-angle) 90) (setf (camera-y-angle camera) 90))
        ((<= (+ (camera-y-angle camera) y-angle) -90) (setf (camera-y-angle camera) -90))
        (t (incf (camera-y-angle camera) y-angle))))

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
  (rotate-object camera (* 5 sensitivity x (/ w)) (* -3 sensitivity y (/ h)))
  (if capture-window (sdl2:warp-mouse-in-window capture-window (/ h 2) (/ w 2))))

;;;; Cubes

;;; Generates 6 faces on a cube from the 8 points on a cube of a given
;;; size with the cube center at OFFSET.
(defun get-cube-points (&key (size 0.5) (offset #(0.0 0.0 0.0 0.0)))
  (let ((point1 (map 'vector #'+ (vector (- size) (- size) (+ size) 1.0) offset))
        (point2 (map 'vector #'+ (vector (+ size) (- size) (+ size) 1.0) offset))
        (point3 (map 'vector #'+ (vector (+ size) (+ size) (+ size) 1.0) offset))
        (point4 (map 'vector #'+ (vector (- size) (+ size) (+ size) 1.0) offset))
        (point5 (map 'vector #'+ (vector (+ size) (+ size) (- size) 1.0) offset))
        (point6 (map 'vector #'+ (vector (- size) (+ size) (- size) 1.0) offset))
        (point7 (map 'vector #'+ (vector (+ size) (- size) (- size) 1.0) offset))
        (point8 (map 'vector #'+ (vector (- size) (- size) (- size) 1.0) offset)))
    (concatenate 'vector
                 point1 point2 point3 point4    ; front
                 point4 point3 point5 point6    ; top
                 point7 point8 point6 point5    ; back
                 point8 point7 point2 point1    ; bottom
                 point8 point1 point4 point6    ; left
                 point2 point7 point5 point3))) ; right

;;; FIXME: This is way too slow to use to actually generate a group of
;;; cubes. Something faster is needed.
(defun get-cube-group (width height depth &key (offset #(0.0 0.0 0.0 0.0)))
  (let ((u nil)
        (v nil)
        (i 0)
        (triangle-points (reduce #'(lambda (x y) (concatenate 'vector x y))
                                 (map 'vector #'(lambda (x y) (map 'vector #'+ x y))
                                      (map 'vector #'(lambda (x) (make-array 6 :initial-element (* 4 x))) #(0 1 2 3 4 5))
                                      (make-array 6 :initial-element #(0 1 2 2 3 0))))))
    (dotimes (x width)
      (dotimes (y height)
        (dotimes (z depth)
          (setf v (concatenate 'vector v (get-cube-points :offset (map 'vector #'+ offset (vector x y z 1.0)))))
          (setf u (concatenate 'vector u (map 'vector #'+
                                              (make-array (* 6 6) :initial-element (* i 6 4))
                                              triangle-points)))
          (incf i))))
    (vector u v)))
