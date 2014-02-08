;;;; Provides matrices not in sb-cga. Also provides some basic
;;;; geometric shapes.

(in-package #:cl-foo)

;;; MATRICES

;;; Implementation of the gluPerspective matrix.
;;; https://www.opengl.org/sdk/docs/man2/xhtml/gluPerspective.xml
(defun perspective-matrix (fovy aspect znear zfar)
  (let ((f (coerce (/ (tan (* fovy (/ pi 360.0)))) 'single-float)))
    (sb-cga:matrix (/ f aspect) 0.0 0.0 0.0
                   0.0 f 0.0 0.0
                   0.0 0.0 (/ (+ zfar znear) (- znear zfar)) (/ (* 2.0 zfar znear) (- znear zfar))
                   0.0 0.0 -1.0 0.0)))

;;; Implementation of the gluLookAt matrix.
;;; https://www.opengl.org/sdk/docs/man2/xhtml/gluLookAt.xml
(defun look-at-matrix (eye target up)
  (let* ((eye (sb-cga:vec (elt eye 0) (elt eye 1) (elt eye 2)))
         (target (sb-cga:vec (elt target 0) (elt target 1) (elt target 2)))
         (up (sb-cga:vec (elt up 0) (elt up 1) (elt up 2)))
         (z (sb-cga:normalize (sb-cga:vec- target eye)))
         (x (sb-cga:cross-product z (sb-cga:normalize up)))
         (y (sb-cga:cross-product (sb-cga:normalize x) z))
         (m1 (sb-cga:matrix (elt x 0) (elt x 1) (elt x 2) 0.0
                            (elt y 0) (elt y 1) (elt y 2) 0.0
                            (- (elt z 0)) (- (elt z 1)) (- (elt z 2)) 0.0
                            0.0 0.0 0.0 1.0))
         (m2 (sb-cga:translate (sb-cga:vec* eye -1.0))))
    (sb-cga:matrix* m2 m1)))

;;; GEOMETRY

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
