;;;; Provides mathematics on matrices and vectors for when graphics
;;;; computations are done in the Lisp code rather than the
;;;; shaders. If these turn out to be very slow, MAP will have to be
;;;; replaced with something else.

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

;;; FIXME: There are probably still mistakes in here. Carefully go
;;; through this.

;;; SPECIAL MATRICES

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
