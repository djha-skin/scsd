;;;; tests/main.lisp

(defpackage #:scsd-test
  (:use #:cl #:fiveam #:scsd/utils #:scsd/conditions)
  ;; Import internal parser symbols needed for testing
  (:import-from #:scsd/parser
                #:database-title-line-p
                #:extract-database-name
                #:description-line-p
                #:table-title-line-p
                #:extract-table-name
                #:pipe-table-line-p
                #:split-pipe-table-line
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
    (is (= (length lines) 4))
    (is (string= (first lines) "# Minimal DB Name"))
    (is (string= (second lines) ""))
    (is (string= (third lines) "This database only has a name and description."))
    (is (string= (fourth lines) "No tables here."))))

;; Tests for Database Title Parsing (Phase 2)
(test database-title-predicate
  "Test the database-title-line-p predicate."
  (is-true (database-title-line-p "# DB Name"))
  (is-true (database-title-line-p "# Another Name "))
  (is-true (database-title-line-p "# name-with-symbols_123"))
  (is-false (database-title-line-p " # Not a title"))
  (is-false (database-title-line-p "#NotATitle"))
  (is-false (database-title-line-p "## Not a DB title"))
  (is-false (database-title-line-p "")))

(test database-name-extraction
  "Test the extract-database-name function."
  (is (string= (extract-database-name "# DB Name") "DB Name"))
  (is (string= (extract-database-name "# Another Name ") "Another Name"))
  (is (string= (extract-database-name "# name-with-symbols_123 ") "name-with-symbols_123")))

(test parse-scsd-db-name-errors
  "Test error handling for missing or malformed DB names in parse-scsd."
  (signals missing-database-title-error
    (parse-scsd "Just some text
No title here"))
  (signals missing-database-title-error
    (parse-scsd "## Not a DB title"))
  (signals missing-database-title-error
    (parse-scsd ""))
  (signals malformed-database-title-error
    (parse-scsd "# "))
  (signals malformed-database-title-error
    (parse-scsd "#    "))
  (finishes (parse-scsd "# Valid Name")))

;; Tests for Database Description Parsing (Phase 3)
(test description-line-predicate
  "Test the description-line-p predicate."
  (is-true (description-line-p "This is a description line."))
  (is-true (description-line-p "  Another description line with leading space."))
  (is-true (description-line-p " Ends with space. "))
  (is-true (description-line-p "Contains # hash and | pipe"))
  (is-false (description-line-p ""))
  (is-false (description-line-p "   "))
  (is-false (description-line-p "# Starts with hash"))
  (is-false (description-line-p " # Starts with hash after space"))
  (is-false (description-line-p "| Starts with pipe"))
  (is-false (description-line-p " | Starts with pipe after space")))

(test parse-scsd-db-description
    "Test database description collection and joining."
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
  (is-false (table-title-line-p "##NotATitle"))
  (is-false (table-title-line-p "# Not a table title"))
  (is-false (table-title-line-p "### Not a table title"))
  (is-false (table-title-line-p "")))

(test table-name-extraction
  "Test the extract-table-name function."
  (is (string= (extract-table-name "## Table Name") "Table Name"))
  (is (string= (extract-table-name "## Another Name ") "Another Name"))
  (is (string= (extract-table-name "## name-with-symbols_123 ") "name-with-symbols_123")))

(test parse-scsd-table-name-errors
  "Test error handling for missing or malformed table names."
  (signals malformed-table-title-error
    (parse-scsd (format nil "# DB Name~%## ~%| C |~%| - |")))
  (finishes (parse-scsd (test-data-path "db_malformed_table")))
  (finishes (parse-scsd (test-data-path "db_invalid_table_marker")))
  (finishes (parse-scsd (test-data-path "db_no_tables")))
  (finishes (parse-scsd (test-data-path "db_single_table")))
  (finishes (parse-scsd (test-data-path "db_multiple_tables"))))

;; Tests for Table Description Parsing (Phase 5)
(test parse-scsd-table-description
  "Test table description collection and joining."
  (skip "parse-scsd does not yet return parsed tables with descriptions")
  #|
  (let ((tables (get-tables (parse-scsd (test-data-path "table_no_desc")))))
    (is (= (length tables) 1))
    (is (string= (get-table-description (first tables)) nil)))
  (let ((tables (get-tables (parse-scsd (test-data-path "table_single_line_desc")))))
    (is (= (length tables) 1))
    (is (string= (get-table-description (first tables)) "This is the table description.")))
  (let ((tables (get-tables (parse-scsd (test-data-path "table_multi_line_desc")))))
    (is (= (length tables) 1))
    (is (string= (get-table-description (first tables))
                 "Line 1.
Line 2.

Line 4 after blank.")))
  |#
  )

;; Tests for Column Header Parsing (Phase 6)
(test header-line-predicate
  "Test the pipe-table-line-p predicate (used for headers etc.)."
  (is-true (pipe-table-line-p "| Header |"))
  (is-true (pipe-table-line-p "| H1 | H2 |"))
  (is-true (pipe-table-line-p "||"))
  (is-false (pipe-table-line-p "| No end pipe"))
  (is-false (pipe-table-line-p "No start pipe |"))
  (is-false (pipe-table-line-p "Not a pipe line"))
  (is-false (pipe-table-line-p "|"))
  (is-false (pipe-table-line-p "")))

(test header-line-split
  "Test splitting header lines, preserving whitespace."
  (is (equal (split-pipe-table-line "| C1 | C2 | C3 |") '(" C1 " " C2 " " C3 ")))
  (is (equal (split-pipe-table-line "|C1|C2|C3|") '("C1" "C2" "C3")))
  (is (equal (split-pipe-table-line "| Col A |  Col B  |   Col C   |") '(" Col A " "  Col B  " "   Col C   ")))
  (is (equal (split-pipe-table-line "| One |") '(" One ")))
  (is (equal (split-pipe-table-line "|||") '("" ""))))

(test parse-scsd-header-errors
  "Test error handling for header issues."
  (signals malformed-header-error
    (parse-scsd (test-data-path "header_empty_col")))
  (signals missing-header-error
    (parse-scsd (test-data-path "header_missing")))
  (signals missing-typespec-error
    (parse-scsd (test-data-path "header_invalid")))
  (finishes (parse-scsd (test-data-path "header_whitespace_cols"))))

;; Tests for Type Specification Parsing (Phase 7)
(test parse-scsd-typespec-errors
  "Test error handling for type specification issues."
  ;; Valid cases
  (finishes (parse-scsd (test-data-path "types_valid")))
  (finishes (parse-scsd (test-data-path "types_whitespace")))

  ;; Missing typespec line
  (signals missing-typespec-error
    (parse-scsd (test-data-path "types_missing")))

  ;; Mismatched count between headers and types
  (signals mismatched-typespec-error
    (parse-scsd (test-data-path "types_mismatch_count")))

  ;; Invalid type marker string
  (signals malformed-typespec-error
    (parse-scsd (test-data-path "types_invalid_marker"))))

;; Placeholder sanity check (can be removed later)
(test sanity-check
  "A basic sanity check to ensure tests run."
  (is (= 1 1)))

;; Function to run all tests in the suite, called by asdf:test-op
(defun run-tests ()
  (run! 'scsd-suite))

