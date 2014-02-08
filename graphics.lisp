;;;; GRAPHICS.LISP
;;; This file takes the relatively low-level APIs of cl-sdl2,
;;; cl-opengl, and sb-cga and turns them into a more abstracted
;;; graphics API so that the rest of the program can use code that
;;; looks more like Lisp code.

(in-package #:cl-foo)

;;;; SDL2
;;; This section abstracts over the SDL2 (and some OpenGL) verbosity,
;;; mainly so that actual programs do not need ridiculous levels of
;;; indentation.

(defmacro with-sdl2 ((window &key (title "CL-FOO") (width 1280) (height 720)) &body body)
  `(sdl2:with-init (:everything)
     (sdl2:with-window (,window :title ,title :w ,width :h ,height :flags '(:shown :opengl))
       (sdl2:with-gl-context (gl-context ,window)
         (sdl2:gl-make-current ,window gl-context)
         (sdl2:hide-cursor)
         (gl:enable :depth-test :cull-face)
         (gl:clear-color 0 0 0 1)
         ,@body))))

(defmacro with-game-loop ((window keydown-actions) &body body)
  `(let ((keydown-scancodes nil))
     (sdl2:with-event-loop (:method :poll)
       (:keydown
        (:keysym keysym)
        (let ((scancode (sdl2:scancode-value keysym)))
          (setf keydown-scancodes (adjoin scancode keydown-scancodes))))

       (:keyup
        (:keysym keysym)
        (let ((scancode (sdl2:scancode-value keysym)))
          (if (member scancode keydown-scancodes)
              (setf keydown-scancodes (set-difference keydown-scancodes (list scancode))))
          (when (sdl2:scancode= scancode :scancode-escape)
            (sdl2:push-event :quit))))

       (:idle
        ()
        (gl:clear :color-buffer :depth-buffer)
        (if keydown-scancodes (map nil ,keydown-actions keydown-scancodes))
        (progn ,@body)
        (gl:flush)
        (sdl2:gl-swap-window ,window))

       (:quit
        ()
        t))))

;;;; OpenGL
;;; This section hides the C-like OpenGL code. It is still very easy
;;; to break things if the order or scope is wrong, and everything
;;; here apparently needs to be within sdl2:with-gl-context (or
;;; with-sdl2) to work.

(defmacro with-buffers ((buffers &key (count 1)) &body body)
  `(let ((,buffers (gl:gen-buffers ,count)))
     (unwind-protect
          (progn ,@body)
       (gl:delete-buffers ,buffers))))

;;; FIXME: Assumes all of the given shaders are used in one program.
(defmacro with-shaders ((shaders program shader-list) &body body)
  `(let* ((,shaders (mapcar #'(lambda (shader)
                                (compile-gl-shader (shader-source shader) (shader-type shader)))
                            ,shader-list))
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

(defmacro with-vao ((program array-buffer element-array-buffer index size count type) &body body)
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
  (gl:uniform-matrix (gl:get-uniform-location program (glsl-name matrix-name)) 4 (vector matrix) nil))

(defun uniform-vector (program vector-name vector)
  (gl:uniformfv (gl:get-uniform-location program (glsl-name vector-name)) vector))

;;;; MATRICES
;;; These provide common matrices that really should be in a graphics
;;; library already.

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
         (up (sb-cga:vec (elt up 0) (elt up 1) (elt up 2)))
         (z (sb-cga:normalize (sb-cga:vec- target eye)))
         (x (sb-cga:cross-product z (sb-cga:normalize up)))
         (y (sb-cga:cross-product (sb-cga:normalize x) z))
         (m1 (sb-cga:matrix (elt x 0) (elt x 1) (elt x 2) 0.0
                            (elt y 0) (elt y 1) (elt y 2) 0.0
                            (- (elt z 0)) (- (elt z 1)) (- (elt z 2)) 0.0
                            0.0 0.0 0.0 1.0))
         (m2 (sb-cga:translate (sb-cga:vec* eye -1.0))))
    (sb-cga:matrix* m2 m1)))
