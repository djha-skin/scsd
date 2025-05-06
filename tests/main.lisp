(in-suite parser-tests)

(test parse-scsd-string-escape-sequences
  ;; Prepare an SCSD string with various escape sequences in string fields
  (let* ((test-data "# Test DB\n## Test Table\n| Text |\n| - |\n| Hello\\nWorld |\n| Tab\\tChar |\n| Quote\\"Char |\n| Backslash\\\\ |\n| Unicode\\u0041 |\n| Unicode\\U00000042 |")
         (temp-file (make-pathname :name "test_escapes" :directory (pathname-directory (test-data-path "")) :type "scsd")))
    ;; Write test data to temp file
    (with-open-file (s temp-file :direction :output :if-exists :supersede :if-does-not-exist :create)
      (format s "~a" test-data))

    ;; Parse the file
    (let ((rows (parse-scsd temp-file)))
      ;; Rows returned in reverse order so fix for test
      (let ((fixed-rows (reverse rows)))
        ;; Check each row's string field for proper escape handling
        (is (string= (first (first fixed-rows)) "Hello\nWorld"))
        (is (string= (first (second fixed-rows)) "Tab\tChar"))
        (is (string= (first (third fixed-rows)) "Quote\"Char"))
        (is (string= (first (fourth fixed-rows)) "Backslash\\"))
        (is (string= (first (fifth fixed-rows)) "UnicodeA"))
        (is (string= (first (sixth fixed-rows)) "UnicodeB"))))))

(in-suite utils-tests)

(test parse-string-with-escapes-basic
  ;; Basic escape sequences test
  (is (string= "Hello\nWorld" (parse-string-with-escapes "Hello\\nWorld")))
  (is (string= "Tab\tChar" (parse-string-with-escapes "Tab\\tChar")))
  (is (string= "Quote\"Char" (parse-string-with-escapes "Quote\\\"Char")))
  (is (string= "Backslash\\" (parse-string-with-escapes "Backslash\\\\"))))

(test parse-string-with-escapes-unicode
  ;; Unicode escape sequences test
  (is (string= "A" (parse-string-with-escapes "\\u0041")))
  (is (string= "B" (parse-string-with-escapes "\\U00000042")))
  ;; Malformed escapes raise error
  (signals error (parse-string-with-escapes "\\uXYZW"))
  (signals error (parse-string-with-escapes "\\U123")))
