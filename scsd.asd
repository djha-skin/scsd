;; scsd.asd
(asdf:defsystem #:scsd
  :description "Source-Control-Storable Database library"
  :author "Skin Shell"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:str #:alexandria)
  :components ((:file "package")
               (:file "src/conditions") ; <-- Load conditions first
               (:file "src/utils")      ; <-- Then utils
               (:file "src/package-parser")
               (:file "src/parser")
               (:file "scsd")))
