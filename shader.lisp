;;;; This file translates s-expressions to GLSL shader strings so that
;;;; shaders can be embedded directly within the source code of the
;;;; file.

(in-package #:cl-foo)

(defun glsl-element (element)
  (cond
    ((null element) nil)
    ((listp element) (make-glsl-operation (elt element 0) (rest element)))
    (t element)))

(defun make-glsl-binary-operation (symbol first-elt rest-elt)
  (ecase symbol
    ((:+) (format nil "(~A~{ + ~A~})" first-elt rest-elt))
    ((:-) (format nil "(~A~{ - ~A~})" first-elt rest-elt))
    ((:*) (format nil "(~A~{ * ~A~})" first-elt rest-elt))
    ((:/) (format nil "(~A~{ / ~A~})" first-elt rest-elt))
    ((:>) (format nil "(~A~{ > ~A~})" first-elt rest-elt))
    ((:<) (format nil "(~A~{ < ~A~})" first-elt rest-elt))
    ((:>=) (format nil "(~A~{ >= ~A~})" first-elt rest-elt))
    ((:<=) (format nil "(~A~{ <= ~A~})" first-elt rest-elt))
    ((:and) (format nil "(~A~{ && ~A~})" first-elt rest-elt))
    ((:or) (format nil "(~A~{ || ~A~})" first-elt rest-elt))))

(defun make-glsl-unary-operation (symbol first-elt)
  (ecase symbol
    ((:not) (format nil "!(~A)" first-elt))
    ((:-) (format nil "-(~A)" first-elt))))

(defun make-glsl-funcall (function-name first-elt rest-elt)
  (if first-elt
      (format nil "~A(~A~{, ~A~})" function-name first-elt rest-elt)
      (format nil "~A()" function-name)))

(defun make-glsl-operation (symbol l)
  (let ((first-elt (glsl-element (first l)))
        (rest-elt (mapcar #'glsl-element (rest l))))
    (case symbol
      ((:+ :* :/ :> :< :>= :<= :and :or) (make-glsl-binary-operation symbol first-elt rest-elt))
      ((:not) (make-glsl-unary-operation symbol first-elt))
      ((:-) (if (= (length rest-elt) 0)
               (make-glsl-unary-operation symbol first-elt)
               (make-glsl-binary-operation symbol first-elt rest-elt)))
      ;; FIXME: Assume everything else is a function call.
      (otherwise (make-glsl-funcall (string-downcase (symbol-name symbol)) first-elt rest-elt)))))

;;; This line is used to define a GLSL variable of a type and name. If
;;; a location integer is given then the proper syntax is provided for
;;; that. If uniform is true, then a proper syntax is given for
;;; that. If in or out is provided under io, then a proper syntax is
;;; given for that.
(defun make-glsl-var (name type &key io location uniform)
  (format nil "~@[layout(location = ~D) ~]~:[~;uniform ~]~@[~A ~]~A ~A;~%" location uniform io type name))

(defun make-glsl-line (l)
  (let ((symbol (elt l 0)))
    (case symbol
      ((:version) (format nil "#version ~D~%~%" (elt l 1)))
      ((:in) (make-glsl-var (elt l 2) (elt l 1) :io "in"))
      ((:out) (make-glsl-var (elt l 2)  (elt l 1) :io "out"))
      ((:in-location) (make-glsl-var (elt l 3) (elt l 2) :io "in" :location (elt l 1)))
      ((:out-location) (make-glsl-var (elt l 3) (elt l 2) :io "out" :location (elt l 1)))
      ((:uniform) (make-glsl-var (elt l 2) (elt l 1) :uniform t))
      ((:setf) (format nil "~A = ~A;~%" (elt l 1) (glsl-element (elt l 2))))
      ((:main) (make-glsl-function "main" (rest l)))
      (otherwise (make-glsl-operation symbol (rest l))))))

;;; FIXME: At the moment doesn't take arguments and doesn't return things.
(defun make-glsl-function (name body)
  (format nil "~%void ~A(void)~%{~%  ~{~A~}}" name (mapcar #'make-glsl-line body)))

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

(defun make-glsl-shader (l)
  (format nil "~{~A~}" (mapcar #'make-glsl-line (cons '(:version 330) l))))
