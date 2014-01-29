;;;; This file contains a variety of functions and macros that help
;;;; with the OpenGL graphics.

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

;;; FIXME: Can I replace this with an algorithm to generate these
;;; coordinates on startup and make the cube's geometry more obvious?
;;; This could then generalize into other geometric shapes perhaps.
(defun get-cube-points ()
  #(-1.0 -1.0 1.0
    1.0 -1.0 1.0
    1.0 1.0 1.0
    -1.0 1.0 1.0

    -1.0 1.0 1.0
    1.0 1.0 1.0
    1.0 1.0 -1.0
    -1.0 1.0 -1.0

    1.0 -1.0 -1.0
    -1.0 -1.0 -1.0
    -1.0 1.0 -1.0
    1.0 1.0 -1.0

    -1.0 -1.0 -1.0
    1.0 -1.0 -1.0
    1.0 -1.0 1.0
    -1.0 -1.0 1.0

    -1.0 -1.0 -1.0
    -1.0 -1.0 1.0
    -1.0 1.0 1.0
    -1.0 1.0 -1.0

    1.0 -1.0 1.0
    1.0 -1.0 -1.0
    1.0 1.0 -1.0
    1.0 1.0 1.0))
