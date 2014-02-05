;;;; This file contains a variety of functions and macros that help
;;;; with the OpenGL graphics.

(in-package #:cl-foo)

;;; Puts a vector into a GL buffer as a GL array.
(defun make-array-buffer (buffer-type buffer array-type vect)
  (let ((array (gl:alloc-gl-array array-type (length vect))))
    (dotimes (i (length vect))
      (setf (gl:glaref array i) (aref vect i)))
    (gl:bind-buffer buffer-type buffer)
    (gl:buffer-data buffer-type :static-draw array)
    (gl:free-gl-array array)
    (gl:bind-buffer buffer-type 0)
    buffer))

(defmacro with-sdl2 ((window &key (title "CL-FOO") (width 1280) (height 720)) &body body)
  `(sdl2:with-init (:everything)
     (sdl2:with-window (,window :title ,title :w ,width :h ,height :flags '(:shown :opengl))
       (sdl2:with-gl-context (gl-context ,window)
         (sdl2:gl-make-current ,window gl-context)
         (sdl2:hide-cursor)
         (gl:enable :depth-test :cull-face)
         (gl:clear-color 0 0 0 1)
         ,@body))))

(defmacro with-buffers ((buffers &key (count 1)) &body body)
  `(let ((,buffers (gl:gen-buffers ,count)))
     (unwind-protect
          (progn ,@body)
       (gl:delete-buffers ,buffers))))

(defmacro with-shaders ((shaders program shader-list) &body body)
  ;; FIXME: Assumes that all of the given shaders are used in the same
  ;; program.
  `(let* ((,shaders (mapcar #'compile-shader ,shader-list))
          (,program (shader-program ,shaders)))
     (unwind-protect
          (progn ,@body)
       (progn
         (map nil #'gl:delete-shader ,shaders)
         (gl:delete-program ,program)))))

(defmacro with-shader-program ((program) &body body)
  `(unwind-protect
        (progn (gl:use-program ,program)
               ,@body)
     (gl:use-program 0)))

(defmacro with-vertex-attrib-array ((program array-buffer element-array-buffer index size count type) &body body)
  `(unwind-protect
        (progn (gl:use-program ,program)
               (gl:bind-buffer :array-buffer ,array-buffer)
               (gl:bind-buffer :element-array-buffer ,element-array-buffer)
               (gl:enable-vertex-attrib-array ,index)
               (gl:vertex-attrib-pointer ,index ,size ,type nil 0 0)
               (gl:bind-vertex-array 0)
               ,@body
               (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short) :count ,count))
     (progn (gl:disable-vertex-attrib-array ,index)
            (gl:bind-buffer :array-buffer 0)
            (gl:bind-buffer :element-array-buffer 0)
            (gl:use-program 0))))

(defun uniform-matrix (program matrix-name matrix)
  (gl:uniform-matrix (gl:get-uniform-location program (glsl-name matrix-name)) 4 (vector matrix)))

(defun uniform-vector (program vector-name vector)
  (gl:uniformfv (gl:get-uniform-location program (glsl-name vector-name)) vector))
