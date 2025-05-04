;;;; scsd-test.asd

(asdf:defsystem #:scsd-test  ; Changed system name
  :description "Test suite for the SCSD library."
  :author "Your Name <your.email@example.com>"
  :license "Specify license here"
  :version "0.0.1"
  ;; Depend on the main system and the testing library
  :depends-on (#:scsd #:fiveam)
  ;; Define how to perform the test operation for this system
  :perform (asdf:test-op (op c) (declare (ignore op))
                         ;; Load the main test file
                         (asdf:load-system :scsd-test) ; Changed system name
                         ;; Run the tests defined in tests/main.lisp
                         (let ((test-fn (find-symbol (string '#:run-tests) '#:scsd-test))) ; Changed package name
                           ;; Check if the function exists before calling
                           (if test-fn
                               (funcall test-fn)
                               (error "Test function run-tests not found in package scsd-test"))))
  ;; Components of the test system
  :components ((:module "tests"
                :components
                ((:file "main")))))
