;;;; GAME.LISP
;;; This file contains more general stuff that doesn't (currently)
;;; belong to another file.

(in-package #:cl-foo)

;;;; CAMERAS

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

;;; Used to define different keyboard layouts for different keyboard
;;; movement systems.
(defmacro scancode-case ((scancode) &rest body)
  (cons 'cond
        (mapcar #'(lambda (l)
                    `((sdl2:scancode= ,scancode ,(elt l 0)) ,(elt l 1)))
                body)))

(defun move-camera (camera scancode)
  (scancode-case (scancode)
                 (:scancode-q (move camera -0.1 :y))
                 (:scancode-e (move camera 0.1 :y))
                 (:scancode-a (move camera -0.1 :x))
                 (:scancode-d (move camera 0.1 :x))
                 (:scancode-s (move camera 0.1 :z))
                 (:scancode-w (move camera -0.1 :z))))

;;;; CUBES

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
