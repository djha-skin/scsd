;;;; tests/main.lisp

(defpackage #:scsd-test
  (:use #:cl #:fiveam #:scsd/utils #:scsd/conditions) ; Removed #:scsd
  ;; Import internal parser symbols needed for testing
  (:import-from #:scsd/parser
                #:database-title-line-p
                #:extract-database-name
                #:parse-scsd ; Use internal parse-scsd for specific error tests
                ))

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

;; Tests for Database Title Parsing (Phase 2)
(test database-title-predicate
  "Test the database-title-line-p predicate."
  (is-true (database-title-line-p "# DB Name"))
  (is-true (database-title-line-p "# Another Name "))
  (is-true (database-title-line-p "# name-with-symbols_123"))
  (is-false (database-title-line-p " # Not a title"))
  (is-false (database-title-line-p "#NotATitle")) ; Missing space
  (is-false (database-title-line-p "## Not a DB title")) ; H2
  (is-false (database-title-line-p "")))

(test database-name-extraction
  "Test the extract-database-name function."
  (is (string= (extract-database-name "# DB Name") "DB Name"))
  (is (string= (extract-database-name "# Another Name ") "Another Name"))
  (is (string= (extract-database-name "# name-with-symbols_123 ") "name-with-symbols_123")))
  ;; Malformed case (empty name) is handled by parse-scsd error, not directly by extractor

(test parse-scsd-db-name-errors
  "Test error handling for missing or malformed DB names in parse-scsd."
  ;; Test missing title error
  (signals missing-database-title-error
    (parse-scsd "Just some text
No title here"))
  (signals missing-database-title-error
    (parse-scsd "## Not a DB title"))
  (signals missing-database-title-error
    (parse-scsd ""))

  ;; Test malformed title error (empty name)
  (signals malformed-database-title-error
    (parse-scsd "# ")) ; Title marker but empty name
  (signals malformed-database-title-error
    (parse-scsd "#    ")) ; Title marker but only whitespace

  ;; Test valid case does NOT signal these errors
  (finishes (parse-scsd "# Valid Name")))

;; Placeholder sanity check (can be removed later)
(test sanity-check
  "A basic sanity check to ensure tests run."
  (is (= 1 1)))

;; Function to run all tests in the suite, called by asdf:test-op
(defun run-tests ()
  (run! 'scsd-suite))

