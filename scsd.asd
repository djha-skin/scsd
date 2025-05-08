;; scsd.asd
(asdf:defsystem #:scsd
  :description "Source-Control-Storable Database library"
  :author "Skin Shell"
  :license "MIT"
  :version "0.0.1"
  :serial t
  :depends-on (#:str #:alexandria)
  :components 
  ((:file "package")
   (:module "src"
    :serial t
    :components
    ((:file "conditions")
     (:file "utils")
     (:file "package-parser") 
     (:file "parser")))
   (:file "scsd")))