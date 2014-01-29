;;;; This file contains a variety of functions and macros that help
;;;; with the OpenGL graphics.

(in-package #:cl-foo)

(defmacro with-buffers ((buffers &key (count 1)) &body body)
  `(let ((,buffers (gl:gen-buffers ,count)))
     (unwind-protect
          (progn ,@body)
       (gl:delete-buffers ,buffers))))

;;; Puts a vector into a GL buffer as a GL array.
(defun gl-array (buffer-type buffer array-type vect)
  (let ((array (gl:alloc-gl-array array-type (length vect))))
    (dotimes (i (length vect))
      (setf (gl:glaref array i) (aref vect i)))
    (gl:bind-buffer buffer-type buffer)
    (gl:buffer-data buffer-type :static-draw array)
    (gl:free-gl-array array)
    (gl:bind-buffer buffer-type 0)
    t))

;;; Each of the 6 faces in a cube is a pair of two triangles who share
;;; the beginning and end points. Each loop here is a face.
(defun get-cube-elements ()
  (let ((v nil))
    (dotimes (i 6)
      (let ((x (* i 4)))
        (setf v (concatenate 'vector v (vector x (+ x 1) (+ x 2) (+ x 2) (+ x 3) x)))))
    v))

;;; Generates 6 faces on a cube from the 8 points on a cube of a given
;;; size with the origin as the cube's center.
(defun get-cube-points (size)
  (let ((point1 (vector (- size) (- size) (+ size)))
        (point2 (vector (+ size) (- size) (+ size)))
        (point3 (vector (+ size) (+ size) (+ size)))
        (point4 (vector (- size) (+ size) (+ size)))
        (point5 (vector (+ size) (+ size) (- size)))
        (point6 (vector (- size) (+ size) (- size)))
        (point7 (vector (+ size) (- size) (- size)))
        (point8 (vector (- size) (- size) (- size))))
    (concatenate 'vector
                 point1 point2 point3 point4    ; front
                 point4 point3 point5 point6    ; top
                 point7 point8 point6 point5    ; back
                 point8 point7 point2 point1    ; bottom
                 point8 point1 point4 point6    ; left
                 point2 point7 point5 point3))) ; right
