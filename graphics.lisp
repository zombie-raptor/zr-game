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
    buffer))

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

(defmacro with-sdl2 ((window &key (title "CL-FOO") (width 1280) (height 720)) &body body)
  `(sdl2:with-init (:everything)
     (sdl2:with-window (,window :title ,title :w ,width :h ,height :flags '(:shown :opengl))
       (sdl2:with-gl-context (gl-context ,window)
         (sdl2:gl-make-current ,window gl-context)
         (sdl2:hide-cursor)
         (gl:enable :depth-test)
         ,@body))))

(defmacro with-vertex-attrib-array ((program array-buffer element-array-buffer index size type) &body body)
  `(unwind-protect
        (progn (gl:use-program ,program)
               (gl:bind-buffer :array-buffer ,array-buffer)
               (gl:bind-buffer :element-array-buffer ,element-array-buffer)
               (gl:enable-vertex-attrib-array ,index)
               (gl:vertex-attrib-pointer ,index ,size ,type nil 0 0)
               (gl:bind-vertex-array 0)
                ,@body)
     (progn (gl:disable-vertex-attrib-array ,index)
            (gl:bind-buffer :array-buffer 0)
            (gl:bind-buffer :element-array-buffer 0)
            (gl:use-program 0))))
