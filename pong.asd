(in-package #:asdf-user)

(asdf:defsystem #:pong
  :serial t
  :description "Pong"
  :author "Joram Schrijver <i@joram.io>"
  :depends-on (#:glop #:cl-opengl #:let-plus #:usocket #:nibbles)
  :components ((:file "package")
               (:file "game")
               (:file "pong")))
