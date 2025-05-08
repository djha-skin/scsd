(defpackage #:scsd-test
  (:use #:cl #:fiveam)
  ;; Import internal symbols needed for testing - adjust as needed
  (:import-from #:scsd/utils #:read-lines #:trim-whitespace #:string-starts-with-p #:join-lines)
  (:import-from #:scsd/parser
                #:database-title-line-p #:extract-database-name
                #:description-line-p
                #:table-title-line-p #:extract-table-name
                #:pipe-table-line-p #:split-pipe-table-line
                #:parse-scsd) ;; Import the main entry point
  (:import-from #:scsd/conditions
                #:scsd-parse-error
                #:missing-database-title-error #:malformed-database-title-error
                #:malformed-table-title-error
                #:missing-header-error
                #:malformed-header-error ;; Although validation is currently off
                #:missing-typespec-error #:mismatched-typespec-error #:malformed-typespec-error
                #:mismatched-field-count-error
                #:malformed-field-error)
  (:import-from #:scsd/parse-string #:parse-string-with-escapes #:char-to-string)
  (:import-from #:alexandria #:read-file-into-string)
  (:export #:run-tests))
(in-package #:scsd-test)

(defun test-data-path (filename)
  (asdf:system-relative-pathname :scsd-test (format nil "tests/data/~a" filename)))

(def-suite scsd-tests
  :description "Top-level test suite for SCSD.")


(def-suite parser-tests
  :description "Tests for the SCSD parser."
  :in scsd-tests)


(in-suite parser-tests)

(test description-line-predicate
  ;;Description of that test function
  (is (string= "hello" (parse-string-with-escapes "hello"))))

(defun run-tests ()
  "Run all defined tests."
  (explain! (run 'scsd-tests)))