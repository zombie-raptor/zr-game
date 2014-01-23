;; Mathematics on matrices and vectors.

(in-package #:cl-foo)

;; Implementation of the gluPerspective matrix.
;; https://www.opengl.org/sdk/docs/man2/xhtml/gluPerspective.xml
(defun perspective-matrix (fovy aspect znear zfar)
  (let ((f (/ (tan (* fovy (/ pi 360.0))))))
    (vector (/ f aspect) 0.0 0.0 0.0
            0.0 f 0.0 0.0
            0.0 0.0 (/ (+ zfar znear) (- znear zfar)) (/ (* 2.0 zfar znear) (- znear zfar))
            0.0 0.0 -1.0 0.0)))

;; Returns scalar from vector.
(defun magnitude (v)
  (sqrt (reduce '+ (map 'vector #'(lambda (x) (expt x 2)) v))))

;; Returns unit vector from vector.
(defun normalize (v)
  (let ((c (magnitude v)))
    (map 'vector #'(lambda (x) (/ x c)) v)))

;; Returns vector from two vectors.
(defun cross-product (u v)
  (vector (- (* (elt u 1) (elt v 2)) (* (elt u 2) (elt v 1)))
          (- (* (elt u 2) (elt v 0)) (* (elt u 0) (elt v 2)))
          (- (* (elt u 0) (elt v 1)) (* (elt u 1) (elt v 0)))))

;; Implementation of the gluLookAt matrix.
;; https://www.opengl.org/sdk/docs/man2/xhtml/gluLookAt.xml
(defun look-at-matrix (eye center up)
  (let* ((f (normalize (map 'vector #'- center eye)))
         (s (cross-product f (normalize up)))
         (u (cross-product (normalize s) f)))
    (vector (elt s 0) (elt s 1) (elt s 2) 0.0
            (elt u 0) (elt u 1) (elt u 2) 0.0
            (- (elt f 0)) (- (elt f 1)) (- (elt f 2)) 0.0
            0.0 0.0 0.0 1.0)))

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

(defun uniform-scale-matrix (c)
  (vector 1 0.0 0.0 0.0
          0.0 1.0 0.0 0.0
          0.0 0.0 1.0 0.0
          0.0 0.0 0.0 (/ c)))
