(in-suite utils-tests)

(test parse-string-with-escapes-basic
  ;; Basic escape sequences test
  (is (string= "Hello\\\"\n\t" (parse-string-with-escapes "Hello\\\"\\n\\t"))))