;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*- vim: set filetype=lisp:

(asdf:defsystem #:com.djha-skin.scsd
    :description "Parser and library for Simple Columnar Store Documents (SCSD)."
    :author "Daniel J. Haskin <djhaskin987@gmail.com>"
    :license "Apache 2.0"
    :version "0.0.1"
    :serial t
    :pathname "cl/"
    :depends-on (#:esrap) ; Add other dependencies here
    :components ((:file "package")
                 (:file "core")      ; Placeholder for core logic
                 (:file "parser"))
    )

(asdf:defsystem #:com.djha-skin.scsd/test
    :description "Test suite for com.djha-skin.scsd."
    :author "Daniel J. Haskin <djhaskin987@gmail.com>"
    :license "Apache 2.0"
    :version "0.0.1"
    :serial t
    :pathname "cl-tests/"
    :depends-on (#:com.djha-skin.scsd ; Depends on the main system
                 #:fiveam)           ; Depends on the testing framework
    :components ((:file "package")   ; Defines com.djha-skin.scsd/test package
                 (:file "parser"))   ; Uses com.djha-skin.scsd/test package
    )
