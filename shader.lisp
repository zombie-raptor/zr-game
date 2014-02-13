;;;; This file translates s-expressions to GLSL shader strings, which
;;;; can then be compiled into OpenGL shaders. It is a work in
;;;; progress.

(in-package #:zr-game)

(defun glsl-name (symbol)
  (let ((camelcase-string (cffi:translate-camelcase-name symbol)))
    (if (cl-ppcre:scan "^gl[A-Z]" camelcase-string)
        (cl-ppcre:regex-replace "^gl" camelcase-string "gl_")
        camelcase-string)))

(defun glsl-element (element)
  (cond
    ((null element) nil)
    ((listp element) (glsl-shader (first element) (rest element)))
    ((symbolp element) (glsl-name element))
    (t element)))

(defun binary-op (symbol first-elt rest-elt)
  (let ((control-string (format nil "(~~A~~{ ~A ~~A~~})" (case symbol
                                                           ((:and) "&&")
                                                           ((:or) "||")
                                                           (otherwise symbol)))))
    (format nil control-string first-elt rest-elt)))

(defun unary-op (symbol-string first-elt)
  (format nil "~A(~A)" symbol-string first-elt))

(defun glsl-function-argument (arg)
  (format nil "~A ~A" (glsl-name (elt arg 1)) (glsl-name (elt arg 0))))

;;; Takes in a name, a type, a list of arguments, and a body and
;;; returns a string representation of a GLSL function. The argument
;;; list can be blank or can be arbitrarily long.
;;;
;;; It takes in typed arguments like defmethod, with each argument
;;; being a list of two symbols, the first as the name and the second
;;; as the type.
(defun glsl-function (name type args body)
  (let ((first-arg (first args))
        (rest-args (rest args)))
    (format nil
            "~%~A ~A(~@[~A~]~{, ~A~})~%{~%~{  ~A~}}"
            (glsl-name type)
            (glsl-name name)
            (if first-arg (glsl-function-argument first-arg))
            (if rest-args (mapcar #'glsl-function-argument rest-args))
            (mapcar #'(lambda (l) (glsl-shader (first l) (rest l))) body))))

;;; This line is used to define a GLSL variable of a type and name. If
;;; a location integer is given then the proper syntax is provided for
;;; that. A storage qualifier is typically given. Usually it is "in", "out", or
;;; "uniform".
;;;
;;; See https://www.opengl.org/wiki/Type_Qualifier_%28GLSL%29#Storage_qualifiers
(defun glsl-var (name type &key storage location)
  (format nil
          "~@[layout(location = ~D) ~]~@[~A ~]~A ~A;~%"
          location
          (if storage (glsl-name storage) nil)
          (glsl-name type)
          (glsl-name name)))

(defun glsl-shader (symbol l)
  (let ((first-elt (first l))
        (rest-elt (rest l)))
    (case symbol
      ((:defun) (glsl-function (elt l 0) (elt l 1) (elt l 2) (nthcdr 3 l)))
      ((:defvar) (glsl-var first-elt (first rest-elt)
                           :storage (getf (rest rest-elt) :storage)
                           :location (getf (rest rest-elt) :location)))
      ((:version) (format nil "#version ~D~%~%" first-elt))
      (otherwise (let ((first-elt (glsl-element first-elt))
                       (rest-elt (mapcar #'glsl-element rest-elt)))
                   (case symbol
                     ((:setf) (format nil "~A = ~A;~%" first-elt (first rest-elt)))
                     ((:+ :* :/ :> :< :>= :<= :and :or) (binary-op symbol first-elt rest-elt))
                     ((:not) (unary-op "!" first-elt))
                     ((:-) (if (= (length rest-elt) 0)
                               (unary-op symbol first-elt)
                               (binary-op symbol first-elt rest-elt)))
                     (otherwise (format nil "~A(~@[~A~]~{, ~A~})" (glsl-name symbol) first-elt rest-elt))))))))

(defun make-glsl-shader (l)
  (format nil "~{~A~}" (mapcar #'(lambda (l) (glsl-shader (first l) (rest l))) l)))

(defclass shader ()
  ((source
    :initarg :source
    :accessor shader-source
    :initform (error "You need to provide the source for the shader."))
   (shader-type
    :initarg :type
    :accessor shader-type
    :initform (error "You need to provide the type for the shader."))))

(defmethod initialize-instance :after ((shader shader) &key)
  (setf (slot-value shader 'source) (make-glsl-shader (slot-value shader 'source))))

(defun compile-all-shaders (shader-list)
  (mapcar #'(lambda (shader)
              (compile-gl-shader (shader-source shader) (shader-type shader)))
          shader-list))
