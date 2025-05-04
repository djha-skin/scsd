(defsystem "scsd"
  :version "0.1.0"
  :author "Daniel Haskin"
  :license "Apache-2.0"
  :description "Parser for Source Control Storable Database format"
  :depends-on (:cl-ppcre :alexandria)
  :components ((:module "src"
                :components
                ((:file "package")
                 (:file "parser" :depends-on ("package")))))
  :in-order-to ((test-op (test-op "scsd/tests"))))

(defsystem "scsd/tests"
  :author "Daniel Haskin"
  :license "Apache-2.0"
  :depends-on (:scsd
               :rove)
  :components ((:module "t"
                :components
                ((:file "parser"))))
  :description "Test system for scsd"
  :perform (test-op (op c) (symbol-call :rove :run c)))