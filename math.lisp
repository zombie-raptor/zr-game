;;;; Provides mathematics on matrices and vectors for when graphics
;;;; computations are done in the Lisp code rather than the
;;;; shaders. If these turn out to be very slow, MAP will have to be
;;;; replaced with something else.
;;;;
;;;; This also provides some geometry, too.

(in-package #:cl-foo)

;;; VECTOR OPERATIONS

;;; Returns scalar from vector.
(defun magnitude (v)
  (sqrt (reduce #'+ (map 'vector #'(lambda (x) (expt x 2)) v))))

;;; Returns unit vector from vector.
(defun normalize (v)
  (let ((c (magnitude v)))
    (map 'vector #'(lambda (x) (/ x c)) v)))

;;; Returns vector from two vectors.
(defun cross-product (u v)
  (vector (- (* (elt u 1) (elt v 2)) (* (elt u 2) (elt v 1)))
          (- (* (elt u 2) (elt v 0)) (* (elt u 0) (elt v 2)))
          (- (* (elt u 0) (elt v 1)) (* (elt u 1) (elt v 0)))))

;;; Returns scalar from two vectors.
(defun dot-product (u v)
  (reduce #'+ (map 'vector #'* u v)))

;;; SPECIAL MATRICES
;;;
;;; FIXME: There are probably still mistakes in here. Carefully go
;;; through this.

;;; Implementation of the gluPerspective matrix.
;;; https://www.opengl.org/sdk/docs/man2/xhtml/gluPerspective.xml
(defun perspective-matrix (fovy aspect znear zfar)
  (let ((f (/ (tan (* fovy (/ pi 360.0))))))
    (vector (/ f aspect) 0.0 0.0 0.0
            0.0 f 0.0 0.0
            0.0 0.0 (/ (+ zfar znear) (- znear zfar)) (/ (* 2.0 zfar znear) (- znear zfar))
            0.0 0.0 -1.0 0.0)))

;;; Implementation of the gluLookAt matrix.
;;; https://www.opengl.org/sdk/docs/man2/xhtml/gluLookAt.xml
(defun look-at-matrix (eye target up)
  (let* ((z (normalize (map 'vector #'- target eye)))
         (x (cross-product z (normalize up)))
         (y (cross-product (normalize x) z))
         (m1 (vector (elt x 0) (elt x 1) (elt x 2) 0
                     (elt y 0) (elt y 1) (elt y 2) 0
                     (- (elt z 0)) (- (elt z 1)) (- (elt z 2)) 0
                     0.0 0.0 0.0 1.0))
         (m2 (translation-matrix (- (elt eye 0)) (- (elt eye 1)) (- (elt eye 2)))))
    (matrix-product m2 m1)))

(defun translation-matrix (x y z)
  (vector 1.0 0.0 0.0 x
          0.0 1.0 0.0 y
          0.0 0.0 1.0 z
          0.0 0.0 0.0 1.0))

(defun scale-matrix (x y z)
  (vector x 0.0 0.0 0.0
          0.0 y 0.0 0.0
          0.0 0.0 z 0.0
          0.0 0.0 0.0 1))

;;; MATRIX OPERATIONS

(defun matrix-product (a b)
  (let ((row-a-0 (vector (elt a 0) (elt a 1) (elt a 2) (elt a 3)))
        (row-a-1 (vector (elt a 4) (elt a 5) (elt a 6) (elt a 7)))
        (row-a-2 (vector (elt a 8) (elt a 9) (elt a 10) (elt a 11)))
        (row-a-3 (vector (elt a 12) (elt a 13) (elt a 14) (elt a 15)))
        (col-b-0 (vector (elt b 0) (elt b 4) (elt b 8) (elt b 12)))
        (col-b-1 (vector (elt b 1) (elt b 5) (elt b 9) (elt b 13)))
        (col-b-2 (vector (elt b 2) (elt b 6) (elt b 10) (elt b 14)))
        (col-b-3 (vector (elt b 3) (elt b 7) (elt b 11) (elt b 15))))
    (vector
     (dot-product row-a-0 col-b-0)
     (dot-product row-a-0 col-b-1)
     (dot-product row-a-0 col-b-2)
     (dot-product row-a-0 col-b-3)
     (dot-product row-a-1 col-b-0)
     (dot-product row-a-1 col-b-1)
     (dot-product row-a-1 col-b-2)
     (dot-product row-a-1 col-b-3)
     (dot-product row-a-2 col-b-0)
     (dot-product row-a-2 col-b-1)
     (dot-product row-a-2 col-b-2)
     (dot-product row-a-2 col-b-3)
     (dot-product row-a-3 col-b-0)
     (dot-product row-a-3 col-b-1)
     (dot-product row-a-3 col-b-2)
     (dot-product row-a-3 col-b-3))))

;;; GEOMETRY

;;; Each of the 6 faces in a cube is a pair of two triangles who share
;;; the beginning and end points. Each loop here is a face in a cube,
;;; and this is repeated for as many cubes as required.
(defun get-cube-elements (number-of-cubes)
  (let ((v nil))
    (dotimes (i (* number-of-cubes 6))
      (let ((x (* i 4)))
        (setf v (concatenate 'vector v (vector x (+ x 1) (+ x 2) (+ x 2) (+ x 3) x)))))
    v))

;;; Generates 6 faces on a cube from the 8 points on a cube of a given
;;; size with the cube center at OFFSET.
(defun get-cube-points (&key (size 1.0) (offset #(0.0 0.0 0.0)))
  (let ((point1 (map 'vector #'+ (vector (- size) (- size) (+ size)) offset))
        (point2 (map 'vector #'+ (vector (+ size) (- size) (+ size)) offset))
        (point3 (map 'vector #'+ (vector (+ size) (+ size) (+ size)) offset))
        (point4 (map 'vector #'+ (vector (- size) (+ size) (+ size)) offset))
        (point5 (map 'vector #'+ (vector (+ size) (+ size) (- size)) offset))
        (point6 (map 'vector #'+ (vector (- size) (+ size) (- size)) offset))
        (point7 (map 'vector #'+ (vector (+ size) (- size) (- size)) offset))
        (point8 (map 'vector #'+ (vector (- size) (- size) (- size)) offset)))
    (concatenate 'vector
                 point1 point2 point3 point4    ; front
                 point4 point3 point5 point6    ; top
                 point7 point8 point6 point5    ; back
                 point8 point7 point2 point1    ; bottom
                 point8 point1 point4 point6    ; left
                 point2 point7 point5 point3))) ; right
