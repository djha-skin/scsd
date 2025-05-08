;; tests/main.lisp
(defpackage #:scsd-test
  (:use #:cl #:fiveam)
  ;; Import internal symbols needed for testing - adjust as needed
  (:import-from #:scsd/utils
                #:read-lines #:trim-whitespace #:string-starts-with-p #:join-lines
                #:unescape-string #:escape-string #:string-contains-unescaped-p)
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
                #:malformed-header-error
                #:missing-typespec-error #:mismatched-typespec-error #:malformed-typespec-error
                #:mismatched-field-count-error
                #:malformed-field-error
                #:malformed-escape-error
                #:incomplete-escape-error
                #:invalid-unicode-escape-error)
  (:import-from #:alexandria #:read-file-into-string)
  (:export #:run-tests))
(in-package #:scsd-test)

(defun test-data-path (filename)
  (asdf:system-relative-pathname :scsd-test (format nil "tests/data/~a" filename)))

(def-suite scsd-tests
  :description "Top-level test suite for SCSD.")

(def-suite utils-tests
  :description "Tests for utility functions."
  :in scsd-tests)

(def-suite escape-handling-tests
  :description "Tests for escape sequence handling."
  :in scsd-tests)

(def-suite parser-tests
  :description "Tests for the SCSD parser."
  :in scsd-tests)

(in-suite utils-tests)

[Previous utility tests remain unchanged...]

(in-suite escape-handling-tests)

(test string-escapes
  ;; Basic character escapes
  (is (string= "test|with|pipes" (unescape-string "test\\|with\\|pipes")))
  (is (string= "test-with-dashes" (unescape-string "test\\-with\\-dashes")))
  (is (string= "test:with:colons" (unescape-string "test\\:with\\:colons")))
  (is (string= "test\\with\\backslash" (unescape-string "test\\\\with\\\\backslash")))
  
  ;; Control character escapes
  (is (string= (format nil "line~%break") (unescape-string "line\\nbreak")))
  (is (string= (format nil "tab~tchar") (unescape-string "tab\\tchar")))
  (let ((all-controls (unescape-string "\\a\\b\\t\\n\\v\\f\\r")))
    (is (= 7 (length all-controls)))
    (is (string= (format nil "~c~c~c~c~c~c~c"
                        #\Bell #\Backspace #\Tab #\Newline #\Vt #\Page #\Return)
                 all-controls)))
  
  ;; Error cases
  (signals incomplete-escape-error
    (unescape-string "incomplete\\"))
  (signals incomplete-escape-error
    (unescape-string "text\\"))
  (finishes 
    (unescape-string "unknown\\escape"))) ; Should just keep the character

(test string-contains-unescaped-p
  (is-true (string-contains-unescaped-p "abc|def" #\|))
  (is-false (string-contains-unescaped-p "abc\\|def" #\|))
  (is-true (string-contains-unescaped-p "abc\\\\|def" #\|)) ; Escaped backslash
  (is-false (string-contains-unescaped-p "no-pipes-here" #\|))
  (is-true (string-contains-unescaped-p "|start" #\|))
  (is-true (string-contains-unescaped-p "end|" #\|)))

(test escape-string
  ;; Basic character escapes
  (is (string= "test\\|with\\|pipes" (escape-string "test|with|pipes")))
  (is (string= "test\\-with\\-dashes" (escape-string "test-with-dashes")))
  (is (string= "test\\:with\\:colons" (escape-string "test:with:colons")))
  (is (string= "test\\\\with\\\\backslash" (escape-string "test\\with\\backslash")))
  
  ;; Control character escapes  
  (is (string= "line\\nbreak" (escape-string (format nil "line~%break"))))
  (is (string= "tab\\tchar" (escape-string (format nil "tab~tchar"))))
  (let ((control-str (format nil "~c~c~c~c~c~c~c"
                            #\Bell #\Backspace #\Tab #\Newline #\Vt #\Page #\Return)))
    (is (string= "\\a\\b\\t\\n\\v\\f\\r" (escape-string control-str))))
  
  ;; Round-trip test
  (let ((original "test|with\\escapes:and-special\\chars"))
    (is (string= original (unescape-string (escape-string original))))))

(test parse-basic-escapes
  ;; Test that the parser properly handles basic escape sequences
  (let ((parsed-rows (parse-scsd (test-data-path "basic_escapes.scsd"))))
    (is (= 2 (length parsed-rows))) ; First table has two rows
    
    ;; Check that pipes were properly escaped in data
    (is (string= "text|with|pipes" (second (first parsed-rows))))
    (is (string= "text-with-dashes" (third (first parsed-rows))))
    (is (string= "text:with:colons" (fourth (first parsed-rows))))
    
    ;; Check second row
    (is (string= "more|escaped|text" (second (second parsed-rows))))
    (is (string= "another-row" (third (second parsed-rows))))
    (is (string= "extra:text" (fourth (second parsed-rows))))))

[Previous test sections remain unchanged...]

(defun ensure-test-files-exist ()
  "Create test files if they don't exist."
  (ensure-directories-exist (test-data-path ""))
  
  ;; Create basic files if they don't exist
  (loop for (filename content) in 
        '(("no_db_title.scsd" "Just some text.")
          ("malformed_db_title.scsd" "# ")
          ("types_invalid_num.scsd" "# DB~%## T~%| Num |~%| -: |~%| not-a-num |")
          ("types_invalid_bool.scsd" "# DB~%## T~%| Bool |~%| :-: |~%| maybe |")
          ;; Add other files...)
        unless (probe-file (test-data-path filename))
          do (with-open-file (s (test-data-path filename)
                               :direction :output
                               :if-does-not-exist :create)
               (write-string content s))))