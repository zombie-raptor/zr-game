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

;;; MATRIX OPERATIONS

;;; fixme: After this point matrices are represented with two
;;; dimensions, as vectors of vectors, with each vector being a
;;; row. Currently, the program expects a one-dimensional vector. The
;;; functions above only return one-dimensional vectors for matrices.

;;; Turns rows into columns.
;;; fixme: make work on arbitrary dimensions
(defun transpose-matrix (m)
  (vector (map 'vector #'(lambda (row) (elt row 0)) m)
          (map 'vector #'(lambda (row) (elt row 1)) m)
          (map 'vector #'(lambda (row) (elt row 2)) m)
          (map 'vector #'(lambda (row) (elt row 3)) m)))

(defun matrix-product (a b)
  ;; Transposes B so columns in B act like rows.
  (let ((b-prime (transpose-matrix b)))
    ;; Computes for every row of A.
    (map 'vector
         #'(lambda (a-row)
             ;; Takes the dot product with every column of B.
             (map 'vector
                  #'(lambda (b-column)
                      (dot-product a-row b-column))
                  b-prime))
         a)))
