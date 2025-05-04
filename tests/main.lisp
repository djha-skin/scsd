;;;; tests/main.lisp

(defpackage #:scsd-test
  (:use #:cl #:fiveam #:scsd #:scsd/utils) ; Added :scsd/utils
  ;; No longer need main :scsd package? Keep for now, might need parse-scsd later
  )

(in-package #:scsd-test)

;; Define the main test suite
(def-suite scsd-suite
  :description "Main test suite for the SCSD library.")

;; Define a helper to get the path to test data files
(defun test-data-path (filename)
  "Returns the pathname for a file in the tests/data/ directory."
  (asdf:system-relative-pathname :scsd-test (make-pathname :directory '(:relative "tests" "data")
                                                             :name filename :type "scsd")))

;; Test basic file reading utility
(in-suite scsd-suite)
(test read-lines-utility
  "Test the read-lines utility function."
  (let ((lines (read-lines (test-data-path "minimal_db"))))
    (is (listp lines))
    (is (= (length lines) 4)) ; Expecting 4 lines based on the file content
    (is (string= (first lines) "# Minimal DB Name"))
    (is (string= (second lines) "")) ; Blank line
    (is (string= (third lines) "This database only has a name and description."))
    (is (string= (fourth lines) "No tables here."))))

;; Placeholder sanity check (can be removed later)
(test sanity-check
  "A basic sanity check to ensure tests run."
  (is (= 1 1)))

;; Function to run all tests in the suite, called by asdf:test-op
(defun run-tests ()
  (run! 'scsd-suite))

