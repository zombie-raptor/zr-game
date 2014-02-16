;;;; This file acts as a layer of abstraction between the graphics
;;;; libraries (cl-opengl cl-sdl2 sb-cga) and the rest of the
;;;; application.

(in-package #:zr-game)

;;;; SDL

;;; SDL doesn't like it if the program is made fullscreen when the
;;; resolution is not the monitor's current resolution.
(defmacro with-sdl2 ((window &key (title "CL-FOO") (width 1280) (height 720) (fullscreen nil)) &body body)
  `(sdl2:with-init (:everything)
     (sdl2:with-window (,window :title ,title :w ,width :h ,height :flags '(:shown :opengl))
       (sdl2:with-gl-context (gl-context ,window)
         (setup-sdl2-and-gl ,window gl-context ,fullscreen)
         ,@body))))

(defmacro with-game-loop ((window keydown-scancodes mouse-motion) &body body)
  `(let ((,keydown-scancodes nil)
         (,mouse-motion (list 0 0)))
     (sdl2:with-event-loop (:method :poll)
       (:keydown
        (:keysym keysym)
        (setf ,keydown-scancodes (keydown-actions ,keydown-scancodes (sdl2:scancode-value keysym))))

       (:keyup
        (:keysym keysym)
        (setf ,keydown-scancodes (keyup-actions ,keydown-scancodes (sdl2:scancode-value keysym))))

       (:mousemotion
        (:xrel xrel :yrel yrel)
        (setf (elt ,mouse-motion 0) xrel)
        (setf (elt ,mouse-motion 1) yrel))

       (:idle
        ()
        (gl:clear :color-buffer :depth-buffer)
        (progn ,@body)
        (gl:flush)
        (sdl2:gl-swap-window ,window))

       (:quit
        ()
        t))))

(defun setup-sdl2-and-gl (window gl-context fullscreen)
  (sdl2:gl-make-current window gl-context)
  (sdl2:hide-cursor)
  (if fullscreen (sdl2:set-window-fullscreen window 1))
  (gl:enable :depth-test :cull-face)
  (gl:clear-color 0 0.1 0.01 1)
  (gl:clear :color-buffer :depth-buffer))

(defun keydown-actions (keydown-scancodes scancode)
  (adjoin scancode keydown-scancodes))

(defun keyup-actions (keydown-scancodes scancode)
  (when (sdl2:scancode= scancode :scancode-escape) (sdl2:push-event :quit))
  (if (member scancode keydown-scancodes)
      (setf keydown-scancodes (set-difference keydown-scancodes (list scancode)))
      keydown-scancodes))

;;; Used to define different keyboard layouts for different keyboard
;;; movement systems.
(defmacro scancode-case ((scancode) &rest body)
  (cons 'cond
        (mapcar #'(lambda (l)
                    `((sdl2:scancode= ,scancode ,(elt l 0)) ,(elt l 1)))
                body)))

;;;; OpenGL

;;;; It is very easy to break OpenGL things if the order or scope is
;;;; wrong. Everything in this section needs to be within
;;;; sdl2:with-gl-context or with-sdl2 to work.

(defgeneric draw (object))

(defmacro with-buffers ((buffers &key (count 1)) &body body)
  `(let ((,buffers (gl:gen-buffers ,count)))
     (unwind-protect
          (progn ,@body)
       (gl:delete-buffers ,buffers))))

;;; FIXME: Assumes all of the given shaders are used in one program.
(defmacro with-shaders ((shaders program shader-list) &body body)
  `(let* ((,shaders (compile-all-shaders ,shader-list))
          (,program (link-gl-program ,shaders)))
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

(defun compile-gl-shader (source type)
  (let ((shader (gl:create-shader type)))
    (gl:shader-source shader source)
    (gl:compile-shader shader)
    (if (not (gl:get-shader shader :compile-status))
        (error (concatenate 'string "Error in compiling shader~%" (gl:get-shader-info-log shader))))
    shader))

(defun link-gl-program (shaders)
  (let ((program (gl:create-program)))
    (map nil #'(lambda (shader) (gl:attach-shader program shader)) shaders)
    (gl:link-program program)
    (if (not (gl:get-program program :link-status))
        (error (concatenate 'string "Error in shader program~%" (gl:get-program-info-log program))))
    (map nil #'(lambda (shader) (gl:detach-shader program shader)) shaders)
    program))

(defun make-array-buffer (buffer-type buffer array-type vect)
  (let ((array (gl:alloc-gl-array array-type (length vect))))
    (dotimes (i (length vect))
      (setf (gl:glaref array i) (aref vect i)))
    (gl:bind-buffer buffer-type buffer)
    (gl:buffer-data buffer-type :static-draw array)
    (gl:free-gl-array array)
    (gl:bind-buffer buffer-type 0)
    buffer))

(defun draw-vao (array-buffer element-array-buffer index count type)
  (unwind-protect
       (progn (gl:bind-buffer :array-buffer array-buffer)
              (gl:bind-buffer :element-array-buffer element-array-buffer)
              (gl:enable-vertex-attrib-array index)
              (gl:vertex-attrib-pointer index 4 type nil 0 0)
              (gl:draw-elements :triangles (gl:make-null-gl-array :unsigned-short) :count count))
    (progn (gl:disable-vertex-attrib-array index)
           (gl:bind-buffer :array-buffer 0)
           (gl:bind-buffer :element-array-buffer 0))))

(defun uniform-matrix (program matrix-name matrix)
  (gl:uniform-matrix (gl:get-uniform-location program (glsl-name matrix-name)) 4 (vector matrix) nil))

(defun uniform-vector (program vector-name vector)
  (gl:uniformfv (gl:get-uniform-location program (glsl-name vector-name)) vector))

(defclass vao ()
  ((array-buffer :initarg :array-buffer)
   (element-array-buffer :initarg :element-array-buffer)
   (array :initarg :array)
   (element-array :initarg :element-array)
   (program :initarg :program)
   (in-variable :initarg :in-variable)))

(defmethod initialize-instance :after ((vao vao) &key)
  (with-shader-program ((slot-value vao 'program))
    (make-array-buffer :array-buffer (slot-value vao 'array-buffer)
                       :float (slot-value vao 'array))
    (make-array-buffer :element-array-buffer (slot-value vao 'element-array-buffer)
                       :unsigned-short (slot-value vao 'element-array))))

(defmethod draw ((vao vao))
  (with-shader-program ((slot-value vao 'program))
    (draw-vao (slot-value vao 'array-buffer)
              (slot-value vao 'element-array-buffer)
              (gl:get-attrib-location (slot-value vao 'program) (glsl-name (slot-value vao 'in-variable)))
              (length (slot-value vao 'element-array)) :float)))

;;;; Matrices

;;; Implementation of the gluPerspective matrix.
;;; https://www.opengl.org/sdk/docs/man2/xhtml/gluPerspective.xml
(defun perspective-matrix (fovy aspect znear zfar)
  (let ((f (coerce (/ (tan (* fovy (/ pi 360.0)))) 'single-float)))
    (sb-cga:matrix (/ f aspect) 0.0 0.0 0.0
                   0.0 f 0.0 0.0
                   0.0 0.0 (/ (+ zfar znear) (- znear zfar)) (/ (* 2.0 zfar znear) (- znear zfar))
                   0.0 0.0 -1.0 0.0)))

;;; Implementation of the gluLookAt matrix.
;;; https://www.opengl.org/sdk/docs/man2/xhtml/gluLookAt.xml
(defun look-at-matrix (eye target up)
  (let* ((eye (sb-cga:vec (elt eye 0) (elt eye 1) (elt eye 2)))
         (target (sb-cga:vec (elt target 0) (elt target 1) (elt target 2)))
         (up (sb-cga:normalize (sb-cga:vec (elt up 0) (elt up 1) (elt up 2))))
         (z (sb-cga:normalize (sb-cga:vec- target eye)))
         (x (sb-cga:cross-product z up))
         (y (sb-cga:cross-product (sb-cga:normalize x) z))
         (m1 (sb-cga:matrix (elt x 0) (elt x 1) (elt x 2) 0.0
                            (elt y 0) (elt y 1) (elt y 2) 0.0
                            (- (elt z 0)) (- (elt z 1)) (- (elt z 2)) 0.0
                            0.0 0.0 0.0 1.0))
         (m2 (sb-cga:translate (sb-cga:vec* eye -1.0))))
    (sb-cga:matrix* m2 m1)))
