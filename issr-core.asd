(defpackage issr-core-asd
  (:use #:cl #:asdf))
(in-package #:issr-core-asd)

(defsystem #:issr-core
  :description "Make Interactive-Server-Side-Rendered web pages with declaritive and recursive programming.
This is the core functionality is reusable for all server modules."
  :author "Charles Jackson <charles.b.jackson@protonmail.com>"
  :license "LLGPL"
  :version "0"
  :serial t
  :depends-on (#:plump
               #:global-vars
               #:str)
  :components ((:file "package")
               (:file "issr-core" :depends-on ("package"))))
