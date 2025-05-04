;;;; tests/main.lisp

(defpackage #:scsd-test ; Changed package name
  (:use #:cl #:fiveam #:scsd))

(in-package #:scsd-test) ; Changed package name

;; Define the main test suite
(def-suite scsd-suite ; Suite name can remain the same or change, doesn't strictly matter
  :description "Main test suite for the SCSD library.")

;; Define a placeholder test (will be replaced later)
(in-suite scsd-suite)
(test sanity-check
  "A basic sanity check to ensure tests run."
  (is (= 1 1)))

;; Function to run all tests in the suite, called by asdf:test-op
(defun run-tests ()
  (run! 'scsd-suite))

