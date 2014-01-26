;;;; This file translates s-expressions to GLSL shader strings so that
;;;; shaders can be embedded directly within the source code of the
;;;; file.

(in-package #:cl-foo)

(defun make-glsl-operator (symbol first rest)
  (case symbol
    ((+) (format nil "(~S~{ + ~S~})" first rest))
    ((-) (if (eq (length rest) 0)
             (format nil "-(~S)" first)
             (format nil "(~S~{ - ~S~})" first rest)))
    ((*) (format nil "(~S~{ * ~S~})" first rest))
    ((/) (format nil "(~S~{ / ~S~})" first rest))
    ((>) (format nil "(~S~{ > ~S~})" first rest))
    ((<) (format nil "(~S~{ < ~S~})" first rest))
    ((>=) (format nil "(~S~{ >= ~S~})" first rest))
    ((<=) (format nil "(~S~{ <= ~S~})" first rest))
    ((not) (format nil "!(~S)" first))
    ((and) (format nil "(~S~{ && ~S~})" first rest))
    ((or) (format nil "(~S~{ || ~S~})" first rest))))

(defun make-glsl-line (l)
  (let ((symbol (nth 0 l)))
    (ccase symbol
      ((+ - * / > < >= <= not and or) (make-glsl-operator symbol (nth 1 l) (nthcdr 2 l)))
      ((in) (format nil "layout(location = ~D) in ~A ~A;~%" (nth 1 l) (nth 2 l) (nth 3 l)))
      ((out) (format nil "layout(location = ~D) out ~A ~A;~%" (nth 1 l) (nth 2 l) (nth 3 l)))
      ((uniform) (format nil "uniform ~A ~A;~%" (nth 1 l) (nth 2 l))))))

(defun read-shader (shader-string shader-type)
  (let ((shader (gl:create-shader shader-type)))
    (gl:shader-source shader shader-string)
    (gl:compile-shader shader)
    (if (not (gl:get-shader shader :compile-status))
        (error (concatenate 'string "Error in compiling shader~%" (gl:get-shader-info-log shader))))
    shader))

(defun shader-program (shaders)
  (let ((program (gl:create-program)))
    (map nil #'(lambda (shader) (gl:attach-shader program shader)) shaders)
    (gl:link-program program)
    (if (not (gl:get-program program :link-status))
        (error (concatenate 'string "Error in shader program~%" (gl:get-program-info-log program))))
    (map nil #'(lambda (shader) (gl:detach-shader program shader)) shaders)
    program))

(defmacro with-shaders ((shaders program &key shader-list shader-type-list) &body body)
  ;; FIXME: Assumes that all of the given shaders are used in the same
  ;; program.
  `(let* ((,shaders (mapcar #'read-shader ,shader-list ,shader-type-list))
          (,program (shader-program ,shaders)))
     (unwind-protect
          (progn ,@body)
       (progn
         (map nil #'gl:delete-shader ,shaders)
         (gl:delete-program ,program)))))
