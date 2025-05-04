;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: COM.DJHA-SKIN.SCSD/TEST; Base: 10 -*-
;;; vim: set filetype=lisp:

;;; Tests for the SCSD parser

(in-package :com.djha-skin.scsd/test)

;;; Define the MAIN test suite for the system
(def-suite :com.djha-skin.scsd.test-suite
    :description "Main test suite for SCSD library.")

;;; Define the test suite for the parser, inside the main suite
(def-suite scsd.parser
    :description "Tests for the SCSD parser."
    :in :com.djha-skin.scsd.test-suite) ; Updated :in clause

(in-suite scsd.parser)

;;; Test parsing of the header section
(test test-parse-header
  (let ((input "[header]
key1: value1
key_2: value-2 with spaces
[/header]
"))
    (let ((result (scsd.low-level:parse-scsd input)))
      (is (equalp result '(:document
                           (:header (("key1" "value1")
                                     ("key_2" "value-2 with spaces")))))))))

;;; Test parsing a table without a caption
(test test-parse-table-no-caption
  (let ((input "[table Simple]
| ColA | ColB |
|------|------|
| r1c1 | r1c2 |
| r2c1 | r2c2 |
[/table]
"))
    (let ((result (scsd.low-level:parse-scsd input)))
      (is (equalp result '(:document
                           ((:table "Simple"
                                    (:header ("ColA" "ColB"))
                                    ((:row ("r1c1" "r1c2"))
                                     (:row ("r2c1" "r2c2")))))))))))

;;; Test parsing a table with a caption
(test test-parse-table-with-caption
  (let ((input "[table WithCaption]
caption: This is a test caption
| Header 1 | Header 2 |
|----------|----------|
| Data 1   | Data 2   |
[/table]
"))
    (let ((result (scsd.low-level:parse-scsd input)))
      (is (equalp result '(:document
                           ((:table "WithCaption"
                                    (:caption "This is a test caption")
                                    (:header ("Header 1" "Header 2"))
                                    ((:row ("Data 1" "Data 2")))))))))))

;;; Test parsing a full document with header and table
(test test-parse-full-document
  (let ((input "[header]
version: 0.1
[/header]

[table Example]
| Foo | Bar |
|-----|-----|
| 1   | 2   |
[/table]
"))
    (let ((result (scsd.low-level:parse-scsd input)))
      (is (equalp result '(:document
                           (:header (("version" "0.1")))
                           ((:table "Example"
                                    (:header ("Foo" "Bar"))
                                    ((:row ("1" "2")))))))))))

;;; Test correct parsing of various VCHARs in header values (Modified Test)
(test test-parse-valuechar-correct
  (let ((input "[header]
key: !"\#$%&'()*+,-./:;<=>?@[\]^_`{|}~
[/header]
")) ; Escaped # and "
    (let ((result (scsd.low-level:parse-scsd input)))
      (is (equalp result '(:document
                           (:header (("key" "!"\#$%&'()*+,-./:;<=>?@[\]^_`{|}~"))))))))) ; Escaped # and "

;;; Test that previously problematic header value now parses (Original error test, now should pass)
(test test-parse-valuechar-original-error-case
  (let ((input "[header]
key: value with spaces
[/header]
"))
    ;; This should now parse correctly with the updated valuechar rule
    (finishes (scsd.low-level:parse-scsd input))
    (let ((result (scsd.low-level:parse-scsd input)))
        (is (equalp result '(:document (:header (("key" "value with spaces")))))))))


;;;---------------------------------------------------------------------------
;;; New Test Suite for Extended Cases
;;;---------------------------------------------------------------------------
(def-suite scsd.parser.extended
    :description "Extended tests for edge cases and specific rules."
    :in scsd.parser) ; This should be nested in scsd.parser

(in-suite scsd.parser.extended)

;;; Test case using the exact example from README.md
(test test-readme-example
  (let ((input "[header]
version: 1.0
author: Jane Doe
[/header]

[table Users]
caption: List of active users
| Name      | Email             | Age |
|-----------|-------------------|-----|
| Alice     | alice@example.com | 30  |
| Bob       | bob@example.com   | 24  |
| Charlie   | char@example.com  | 29  |
[/table]

[table Products]
| ID  | Name      | Price |
|-----|-----------|-------|
| 1   | Widget    | 10.99 |
| 2   | Gadget    | 25.50 |
[/table]"))
    (let ((result (scsd.low-level:parse-scsd input)))
      (is (equalp result '(:document
                           (:header (("version" "1.0") ("author" "Jane Doe")))
                           ((:table "Users"
                                    (:caption "List of active users")
                                    (:header ("Name" "Email" "Age"))
                                    ((:row ("Alice" "alice@example.com" "30"))
                                     (:row ("Bob" "bob@example.com" "24"))
                                     (:row ("Charlie" "char@example.com" "29"))))
                            (:table "Products"
                                    (:header ("ID" "Name" "Price"))
                                    ((:row ("1" "Widget" "10.99"))
                                     (:row ("2" "Gadget" "25.50")))))))))))

;;; Test table separator variations
(test test-table-separator
  ;; Minimal separator
  (let ((input "[table Sep1]
| A |
|-|
| 1 |
[/table]
"))
    (finishes (scsd.low-level:parse-scsd input)))
  ;; Longer separator
  (let ((input "[table Sep2]
| ColA | ColB |
|------|------|
| r1c1 | r1c2 |
[/table]
"))
    (finishes (scsd.low-level:parse-scsd input)))
  ;; Separator with different lengths
  (let ((input "[table Sep3]
| C1 | C2 |
|--|---------|
| v1 | v2 |
[/table]
"))
    (finishes (scsd.low-level:parse-scsd input)))
  ;; Separator must have at least one '-'? Rule is (+ #\-) -> yes.
  (signals esrap:esrap-parse-error
    (scsd.low-level:parse-scsd "[table SepErr]
| A |
||
| 1 |
[/table]
")))

;;; Test cell/column content variations (trimming, VCHARs)
(test test-cell-content
  (let ((input "[table Content]
| Leading | Trailing | Both | Special !@#$ |
|---------|----------|------|--------------|
|  Space  | Space    | Spc  | !@#$         |
[/table]
"))
    (let ((result (scsd.low-level:parse-scsd input)))
      (is (equalp result '(:document
                           ((:table "Content"
                                    (:header ("Leading" "Trailing" "Both" "Special !@#$"))
                                    ((:row ("Space" "Space" "Spc" "!@#$")))))))))))

;;; Test empty table (header and separator only)
(test test-empty-table
  (let ((input "[table Empty]
| Header |
|--------|
[/table]
"))
    (let ((result (scsd.low-level:parse-scsd input)))
      (is (equalp result '(:document
                           ((:table "Empty"
                                    (:header ("Header"))
                                    ()))))))) ; Empty list of rows

;;; Test table with zero columns (allowed by '*' in rules)
(test test-zero-column-table
    (let ((input "[table ZeroCol]
||
|-|
||
[/table]
"))
      (let ((result (scsd.low-level:parse-scsd input)))
        (is (equalp result '(:document
                             ((:table "ZeroCol"
                                      (:header ()) ; Zero columns
                                      ((:row ())))))))))) ; One row, zero cells

;;; Test document with only a header
(test test-header-only
  (let ((input "[header]
key: val
[/header]"))
    (let ((result (scsd.low-level:parse-scsd input)))
      (is (equalp result '(:document (:header (("key" "val")))))))))

;;; Test document with only a table
(test test-table-only
  (let ((input "[table JustTable]
| H |
|-|
| D |
[/table]"))
    (let ((result (scsd.low-level:parse-scsd input)))
      (is (equalp result '(:document ((:table "JustTable" (:header ("H")) ((:row ("D")))))))))))

;;; Test empty input string
(test test-empty-input
  (let ((input ""))
    (let ((result (scsd.low-level:parse-scsd input)))
      (is (equalp result '(:document ())))))) ; Document with no header, no tables

;;; Test whitespace only input string
(test test-whitespace-input
  (let ((input "  
	
  "))
    (let ((result (scsd.low-level:parse-scsd input)))
      (is (equalp result '(:document ()))))))
)