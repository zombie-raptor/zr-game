;;;; package.lisp

(defpackage #:cl-foo
  (:use #:cl #:sdl2 #:alexandria)
  (:export #:main-loop
           #:perspective-matrix
           #:look-at-matrix
           #:translation-matrix
           #:scale-matrix
           #:uniform-scale-matrix))
