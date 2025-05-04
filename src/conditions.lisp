;;;; src/conditions.lisp

(defpackage #:scsd/conditions
  (:use #:cl)
  (:export #:scsd-parse-error
           #:line-number
           #:missing-database-title-error
           #:malformed-database-title-error
           #:malformed-table-title-error
           #:malformed-header-error
           #:malformed-typespec-error ; Export new
           #:mismatched-typespec-error ; Export new
           #:missing-header-error ; Placeholder for previous TODO
           #:missing-typespec-error ; Placeholder
           ))

(in-package #:scsd/conditions)

(define-condition scsd-parse-error (simple-error)
  ((line-number :initarg :line-number :reader line-number :initform nil))
  (:report (lambda (condition stream)
             (format stream "SCSD Parse Error~@[ on line ~A~]: ~?~%"
                     (line-number condition)
                     (simple-condition-format-control condition)
                     (simple-condition-format-arguments condition)))))

(define-condition missing-database-title-error (scsd-parse-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Missing Database Title: SCSD document must start with a database title line (e.g., '# Database Name').~@[ Error near line ~A.~]"
                     (line-number condition))))
  (:default-initargs :format-control "Missing Database Title"))

(define-condition malformed-database-title-error (scsd-parse-error)
  ((title-line :initarg :title-line :reader title-line)) ; Store the problematic line
  (:report (lambda (condition stream)
             (format stream "Malformed Database Title: Title line found, but database name is empty or invalid. Line ~A: ~S"
                     (line-number condition)
                     (title-line condition))))
  (:default-initargs :format-control "Malformed Database Title"))

(define-condition malformed-table-title-error (scsd-parse-error)
  ((title-line :initarg :title-line :reader title-line)) ; Store the problematic line
  (:report (lambda (condition stream)
             (format stream "Malformed Table Title: Title line found, but table name is empty or invalid. Line ~A: ~S"
                     (line-number condition)
                     (title-line condition))))
  (:default-initargs :format-control "Malformed Table Title"))

(define-condition malformed-header-error (scsd-parse-error)
  ((header-line :initarg :header-line :reader header-line)
   (reason :initarg :reason :reader reason))
  (:report (lambda (condition stream)
             (format stream "Malformed Header Line: ~A. Line ~A: ~S"
                     (reason condition)
                     (line-number condition)
                     (header-line condition))))
  (:default-initargs :format-control "Malformed Header Line"))

(define-condition missing-header-error (scsd-parse-error) ; Added
  ((table-name :initarg :table-name :reader table-name))
  (:report (lambda (condition stream)
             (format stream "Missing or invalid table header line for table '~A'.~@[ Error near line ~A.~]"
                     (table-name condition)
                     (line-number condition))))
  (:default-initargs :format-control "Missing or invalid table header line"))

(define-condition missing-typespec-error (scsd-parse-error) ; Added
  ((table-name :initarg :table-name :reader table-name))
  (:report (lambda (condition stream)
             (format stream "Missing or invalid type specification line for table '~A'.~@[ Error near line ~A.~]"
                     (table-name condition)
                     (line-number condition))))
  (:default-initargs :format-control "Missing or invalid type specification line"))

(define-condition malformed-typespec-error (scsd-parse-error) ; Added
  ((typespec-line :initarg :typespec-line :reader typespec-line)
   (reason :initarg :reason :reader reason))
  (:report (lambda (condition stream)
             (format stream "Malformed Type Specification Line: ~A. Line ~A: ~S"
                     (reason condition)
                     (line-number condition)
                     (typespec-line condition))))
  (:default-initargs :format-control "Malformed Type Specification Line"))

(define-condition mismatched-typespec-error (malformed-typespec-error) ; Added
  ((header-count :initarg :header-count :reader header-count)
   (typespec-count :initarg :typespec-count :reader typespec-count))
  (:report (lambda (condition stream)
             (format stream "Malformed Type Specification Line: Number of types (~A) does not match number of headers (~A). Line ~A: ~S"
                     (typespec-count condition)
                     (header-count condition)
                     (line-number condition)
                     (typespec-line condition))))
  (:default-initargs :format-control "Type specification count does not match header count"))

