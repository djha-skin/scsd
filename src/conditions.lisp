;;;; src/conditions.lisp

(defpackage #:scsd/conditions
  (:use #:cl)
  (:export #:scsd-parse-error
           #:line-number
           #:missing-database-title-error
           #:malformed-database-title-error
           #:malformed-table-title-error
           #:malformed-header-error
           #:malformed-typespec-error
           #:mismatched-typespec-error
           #:missing-header-error
           #:missing-typespec-error
           #:mismatched-field-count-error
           #:malformed-field-error
           #:invalid-number-format-error
           ;; New escape sequence related conditions
           #:malformed-escape-error
           #:incomplete-escape-error
           #:invalid-unicode-escape-error))

(in-package #:scsd/conditions)

(define-condition scsd-parse-error (simple-error)
  ((line-number :initarg :line-number :reader line-number :initform nil))
  (:report (lambda (condition stream)
             (format stream "SCSD Parse Error~@[ on line ~A~]: ~?~%"
                     (line-number condition)
                     (simple-condition-format-control condition)
                     (simple-condition-format-arguments condition)))))

;; Keep all existing base conditions first
;; Then add new escape sequence related ones at the end

(define-condition missing-database-title-error (scsd-parse-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Missing Database Title: SCSD document must start with a database title line (e.g., '# Database Name').~@[ Error near line ~A.~]"
                     (line-number condition))))
  (:default-initargs :format-control "Missing Database Title"))

(define-condition malformed-database-title-error (scsd-parse-error)
  ((title-line :initarg :title-line :reader title-line))
  (:report (lambda (condition stream)
             (format stream "Malformed Database Title: Title line found, but database name is empty or invalid. Line ~A: ~S"
                     (line-number condition)
                     (title-line condition))))
  (:default-initargs :format-control "Malformed Database Title"))

(define-condition malformed-table-title-error (scsd-parse-error)
  ((title-line :initarg :title-line :reader title-line))
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

(define-condition missing-header-error (scsd-parse-error)
  ((table-name :initarg :table-name :reader table-name))
  (:report (lambda (condition stream)
             (format stream "Missing or invalid table header line for table '~A'.~@[ Error near line ~A.~]"
                     (table-name condition)
                     (line-number condition))))
  (:default-initargs :format-control "Missing or invalid table header line"))

(define-condition missing-typespec-error (scsd-parse-error)
  ((table-name :initarg :table-name :reader table-name))
  (:report (lambda (condition stream)
             (format stream "Missing or invalid type specification line for table '~A'.~@[ Error near line ~A.~]"
                     (table-name condition)
                     (line-number condition))))
  (:default-initargs :format-control "Missing or invalid type specification line"))

(define-condition malformed-typespec-error (scsd-parse-error)
  ((typespec-line :initarg :typespec-line :reader typespec-line)
   (reason :initarg :reason :reader reason))
  (:report (lambda (condition stream)
             (format stream "Malformed Type Specification Line: ~A. Line ~A: ~S"
                     (reason condition)
                     (line-number condition)
                     (typespec-line condition))))
  (:default-initargs :format-control "Malformed Type Specification Line"))

(define-condition mismatched-typespec-error (malformed-typespec-error)
  ((header-count :initarg :header-count :reader header-count)
   (typespec-count :initarg :typespec-count :reader typespec-count))
  (:report (lambda (condition stream)
             (format stream "Malformed Type Specification Line: Number of types (~A) does not match number of headers (~A). Line ~A: ~S"
                     (typespec-count condition)
                     (header-count condition)
                     (line-number condition)
                     (typespec-line condition))))
  (:default-initargs :format-control "Type specification count does not match header count"))

(define-condition mismatched-field-count-error (scsd-parse-error)
  ((data-line :initarg :data-line :reader data-line)
   (header-count :initarg :header-count :reader header-count)
   (field-count :initarg :field-count :reader field-count))
  (:report (lambda (condition stream)
             (format stream "Mismatched Field Count: Row has ~A field(s) but header specifies ~A column(s). Line ~A: ~S"
                     (field-count condition)
                     (header-count condition)
                     (line-number condition)
                     (data-line condition))))
  (:default-initargs :format-control "Data row field count does not match header column count"))

(define-condition malformed-field-error (scsd-parse-error)
  ((field-value :initarg :field-value :reader field-value)
   (reason :initarg :reason :reader reason))
  (:report (lambda (condition stream)
             (format stream "Malformed Field Value: ~A. Field: ~S~@[ Line: ~A~]"
                     (reason condition)
                     (field-value condition)
                     (line-number condition))))
  (:default-initargs :format-control "Malformed Field Value"))

(define-condition invalid-number-format-error (malformed-field-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Invalid Number Format: Cannot parse ~S as number.~@[ Line: ~A~]"
                     (field-value condition)
                     (line-number condition))))
  (:default-initargs :format-control "Invalid number format"))

(define-condition malformed-escape-error (scsd-parse-error)
  ((escape-sequence :initarg :escape-sequence :reader escape-sequence :initform nil))
  (:report (lambda (condition stream)
             (format stream "Malformed escape sequence~@[: Invalid escape '~A'~].~@[ Line: ~A~]"
                     (escape-sequence condition)
                     (line-number condition))))
  (:default-initargs :format-control "Malformed escape sequence"))

(define-condition incomplete-escape-error (malformed-escape-error)
  ()
  (:report (lambda (condition stream)
             (format stream "Incomplete escape sequence: String ends with a backslash.~@[ Line: ~A~]"
                     (line-number condition))))
  (:default-initargs :format-control "Incomplete escape sequence"))

(define-condition invalid-unicode-escape-error (malformed-escape-error)
  ((unicode-sequence :initarg :unicode-sequence :reader unicode-sequence :initform nil))
  (:report (lambda (condition stream)
             (format stream "Invalid Unicode escape sequence~@[ '~A'~]: Must be \\u followed by exactly 4 hex digits.~@[ Line: ~A~]"
                     (unicode-sequence condition)
                     (line-number condition))))
  (:default-initargs :format-control "Invalid Unicode escape sequence"))