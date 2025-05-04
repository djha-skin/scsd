;;;; scsd.asd

(asdf:defsystem #:scsd
  :description "Library for parsing, serializing, and manipulating SCSD format data."
  :author "Your Name <your.email@example.com>" ; Placeholder - update later if needed
  :license "Specify license here" ; Placeholder - update later if needed
  :version "0.0.1"
  :serial t
  :components ((:file "package") ; Load main package definition first
               (:module "src"     ; Define src module
                :serial t
                :components ((:file "utils")          ; Load utils
                             (:file "conditions")     ; Load conditions BEFORE parser package
                             (:file "package-parser") ; Load parser package def
                             (:file "parser")         ; Load parser logic
                             ;; Add other source files here later
                             ))
               ;; Load main scsd file last to handle re-exports if necessary
               (:file "scsd")))
