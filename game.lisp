;;;; This file contains more general stuff that doesn't (currently)
;;;; belong to another file.

(in-package #:zr-game)

;;;; Cameras

(defgeneric get-matrix (object))
(defgeneric move (object magnitude direction))
(defgeneric rotate-object (object x-z-angle y-angle))

(defclass camera ()
  ((direction
    :accessor camera-direction
    :initform #(0.0 0.0 -1.0))
   (up
    :accessor camera-up
    :initform #(0.0 1.0 0.0))
   (x-z-angle
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
    :initform #(0.0 0.0 0.0 1.0))))

(defmethod initialize-instance :after ((camera camera) &key)
  (rotate-object camera 0.0 0.0))

(defmethod get-matrix ((camera camera))
  (camera-matrix (camera-direction camera) (camera-up camera)))

;; FIXME: Movement on X/Z is currently constant based on the initial
;; starting point, no matter which way the camera is facing, sort of
;; like a tank turret. Movement should be relative to the angle on the
;; x-z plane where you are looking, although Y should be
;; independent. I think this would provide FPS-style movement.
(defmethod move ((camera camera) magnitude direction)
  (let ((i (case direction
             ((:x) 0)
             ((:y) 1)
             ((:z) 2))))
    (incf (elt (world-offset camera) i) (- magnitude))))

(defmethod rotate-object ((camera camera) x-z-angle y-angle)
  (incf (camera-x-z-angle camera) x-z-angle)
  (when (and (<= (+ (camera-y-angle camera) y-angle) 90)
             (>= (+ (camera-y-angle camera) y-angle) -90)
             (incf (camera-y-angle camera) y-angle)))
  (setf (camera-direction camera) (map 'vector #'(lambda (x) (coerce x 'single-float))
                                       (vector (cos (* (camera-x-z-angle camera) pi (/ 180)))
                                               (sin (* (camera-y-angle camera) pi (/ 180)))
                                               (sin (* (camera-x-z-angle camera) pi (/ 180)))))))

(defun move-camera (camera scancode)
  (scancode-case (scancode)
                 (:scancode-t (rotate-object camera 0 1))
                 (:scancode-g (rotate-object camera 0 -1))
                 (:scancode-f (rotate-object camera -1 0))
                 (:scancode-h (rotate-object camera 1 0))
                 (:scancode-q (move camera -0.1 :y))
                 (:scancode-e (move camera 0.1 :y))
                 (:scancode-a (move camera -0.1 :x))
                 (:scancode-d (move camera 0.1 :x))
                 (:scancode-s (move camera 0.1 :z))
                 (:scancode-w (move camera -0.1 :z))))

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
