;;;; tests/main.lisp

(defpackage #:scsd-test
  (:use #:cl #:fiveam #:scsd/utils #:scsd/conditions)
  ;; Import internal parser symbols needed for testing
  (:import-from #:scsd/parser
                #:database-title-line-p
                #:extract-database-name
                #:description-line-p
                #:table-title-line-p ; Import new
                #:extract-table-name ; Import new
                #:parse-scsd
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

;; Tests for Database Description Parsing (Phase 3)
(test description-line-predicate
  "Test the description-line-p predicate."
  (is-true (description-line-p "This is a description line."))
  (is-true (description-line-p "  Another description line with leading space."))
  (is-true (description-line-p " Ends with space. "))
  (is-true (description-line-p "Contains # hash and | pipe"))
  (is-false (description-line-p ""))
  (is-false (description-line-p "   ")) ; Whitespace only
  (is-false (description-line-p "# Starts with hash"))
  (is-false (description-line-p " # Starts with hash after space"))
  (is-false (description-line-p "| Starts with pipe"))
  (is-false (description-line-p " | Starts with pipe after space")))

(test parse-scsd-db-description ; Placeholder - Need parse-scsd to return description
    "Test database description collection and joining."
    ;; These tests are placeholders until parse-scsd returns the description
    ;; Need to modify parse-scsd return value or create a helper for testing
    (skip "parse-scsd does not yet return parsed description")
    #|
    (is (string= (get-db-description (parse-scsd (test-data-path "db_no_desc"))) nil))
    (is (string= (get-db-description (parse-scsd (test-data-path "db_single_line_desc")))
                   "This is the description."))
    (is (string= (get-db-description (parse-scsd (test-data-path "db_multi_line_desc")))
                   "This is the first line.
This is the second line.

This is after a blank line."))
    (is (string= (get-db-description (parse-scsd (test-data-path "db_desc_leading_whitespace")))
                   "  This description starts with whitespace.
Still part of it."))
    |#
    )

;; Tests for Table Name Parsing (Phase 4)
(test table-title-predicate
  "Test the table-title-line-p predicate."
  (is-true (table-title-line-p "## Table Name"))
  (is-true (table-title-line-p "## Another Name "))
  (is-true (table-title-line-p "## name-with-symbols_123"))
  (is-false (table-title-line-p " ## Not a title"))
  (is-false (table-title-line-p "##NotATitle")) ; Missing space
  (is-false (table-title-line-p "# Not a table title")) ; H1
  (is-false (table-title-line-p "### Not a table title")) ; H3
  (is-false (table-title-line-p "")))

(test table-name-extraction
  "Test the extract-table-name function."
  (is (string= (extract-table-name "## Table Name") "Table Name"))
  (is (string= (extract-table-name "## Another Name ") "Another Name"))
  (is (string= (extract-table-name "## name-with-symbols_123 ") "name-with-symbols_123")))
  ;; Malformed case (empty name) is handled by parse-scsd error

(test parse-scsd-table-name-errors
  "Test error handling for missing or malformed table names."
  ;; Malformed table name (empty name after ##<space>)
  (signals malformed-table-title-error
    (parse-scsd (format nil "# DB Name~%## ~%| C |~%| - |"))) ; Use format to create string input

  ;; Invalid marker (## without space) - should NOT be seen as a table title
  ;; Currently results in "Unexpected content" warning, should finish parsing before tables.
  (finishes (parse-scsd (test-data-path "db_malformed_table"))) ; Changed expectation

  ;; Invalid marker (###) - should NOT be seen as a table title
  (finishes (parse-scsd (test-data-path "db_invalid_table_marker")))

  ;; Valid cases (should finish without table name errors)
  (finishes (parse-scsd (test-data-path "db_no_tables")))
  (finishes (parse-scsd (test-data-path "db_single_table")))
  (finishes (parse-scsd (test-data-path "db_multiple_tables"))))


;; Placeholder sanity check (can be removed later)
(test sanity-check
  "A basic sanity check to ensure tests run."
  (is (= 1 1)))

;; Function to run all tests in the suite, called by asdf:test-op
(defun run-tests ()
  (run! 'scsd-suite))

