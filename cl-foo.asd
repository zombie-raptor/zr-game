;;;; cl-foo.asd

(asdf:defsystem #:cl-foo
  :serial t
  :description "Describe cl-foo here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :components ((:file "package")
               (:file "cl-foo")))

