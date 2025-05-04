(defpackage #:scsd/tests
  (:use #:cl
        #:scsd
        #:rove))
(in-package #:scsd/tests)

;; Utilities
(defun make-string-input-stream-from-lines (&rest lines)
  (make-string-input-stream (format nil "~{~A~%~}" lines)))

(deftest test-database-title
  (testing "should parse database title"
    (let ((input (make-string-input-stream-from-lines "# test-db")))
      (ok (equal "test-db"
                 (scsd-database-name (parse-scsd input))))))

  (testing "should error on invalid database title"
    (let ((input (make-string-input-stream-from-lines "not a title")))
      (ok (signals (parse-scsd input) 'scsd-parse-error))))

  (testing "should allow description"
    (let* ((input (make-string-input-stream-from-lines 
                   "# test-db"
                   "This is a description"
                   "It can span multiple lines"))
           (db (parse-scsd input)))
      (ok (equal "test-db" (scsd-database-name db)))
      (ok (equal '("This is a description" "It can span multiple lines")
                 (nreverse (scsd-database-description db)))))))