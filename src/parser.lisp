(in-package :scsd)

;; Conditions
(define-condition scsd-error (error)
  ((message :initarg :message :reader error-message))
  (:report (lambda (condition stream)
            (format stream "SCSD Error: ~A" (error-message condition)))))

(define-condition scsd-parse-error (scsd-error)
  ((line :initarg :line :reader error-line)
   (position :initarg :position :reader error-position))
  (:report (lambda (condition stream)
            (format stream "SCSD Parse Error at line ~A position ~A: ~A"
                    (error-line condition)
                    (error-position condition)
                    (error-message condition)))))

;; Structures
(defstruct scsd-database
  name
  description
  tables)

(defstruct scsd-table
  name
  description
  columns
  column-types
  rows)

;; Utilities
(defun split-line (line)
  "Split a line on pipe characters, removing the first and last empty strings"
  (let ((parts (cl-ppcre:split "\\|" line)))
    (if (>= (length parts) 2)
        (subseq parts 1 (1- (length parts)))
        nil)))

(defun parse-line-separator (line)
  "Returns true if the line is empty or only contains whitespace"
  (cl-ppcre:scan "^\\s*$" line))

(defun parse-database-title (line)
  "Parse a line that should be a database title (# name)"
  (when (cl-ppcre:scan "^# \\S" line)
    (string-trim " " (subseq line 2))))

(defun parse-table-title (line)
  "Parse a line that should be a table title (## name)"
  (when (cl-ppcre:scan "^## \\S" line)
    (string-trim " " (subseq line 3))))

(defparameter *column-type-patterns*
  '(("-" . :string)
    (":-" . :keyword)
    ("-:" . :number)
    (":-:" . :boolean)))

(defun parse-column-type (type-str)
  "Parse a column type specification"
  (or (cdr (assoc type-str *column-type-patterns* :test #'string=))
      (error 'scsd-parse-error 
             :message (format nil "Invalid column type: ~A" type-str)
             :line 0 :position 0)))

;; Main parsing functions
(defun parse-scsd (stream)
  "Parse an SCSD database from a stream"
  (let ((database (make-scsd-database))
        (current-line 0)
        (state :initial))
    (loop for line = (read-line stream nil nil)
          while line
          do (incf current-line)
             (cond
               ;; Initial state - looking for database title
               ((eq state :initial)
                (if-let (name (parse-database-title line))
                  (progn
                    (setf (scsd-database-name database) name)
                    (setf state :description))
                  (unless (parse-line-separator line)
                    (error 'scsd-parse-error 
                           :message "Expected database title"
                           :line current-line
                           :position 0))))
               
               ;; Description state - collecting description lines until table or empty line
               ((eq state :description)
                (cond
                  ((parse-table-title line)
                   (setf state :table)
                   ;; TODO: Handle table parsing)
                  ((not (parse-line-separator line))
                   (push line 
                         (scsd-database-description database)))))))
    database))