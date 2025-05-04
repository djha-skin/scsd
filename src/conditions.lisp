;;;; src/conditions.lisp

(defpackage #:scsd/conditions
  (:use #:cl)
  (:export #:scsd-parse-error          ; Base condition for all parse errors
           #:line-number              ; Reader for line number slot
           #:missing-database-title-error ; Specific error
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

