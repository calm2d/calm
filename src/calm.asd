#+sbcl (sb-ext:unlock-package :sb-ext)
#+sbcl
(defmethod asdf:perform ((o asdf:image-op) (c asdf:system))
  (uiop:dump-image (asdf:output-file o c)
                   :executable t))

(asdf:defsystem #:calm
  :description "CALM - Canvas And Lisp Magic"
  :version "0.0.11"
  :author "Vito Van"
  :license "Mozilla Public License Version 2.0"
  :depends-on (#:sdl2
               #:sdl2-mixer
               #:str
               #:swank
               #:cl-cairo2)
  :pathname "./"
  :serial t
  :components ((:file "package")
               (:file "calm"))
  :build-operation "program-op"
  :build-pathname "calm"
  :entry-point "calm:entry")
