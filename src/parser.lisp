;;;; src/parser.lisp

(in-package #:scsd/parser)

;;; Predicates for identifying line types

(defun database-title-line-p (line)
  "Checks if a line starts with '# ' signifying a database title."
  (string-starts-with-p line "# "))

(defun table-title-line-p (line)
  "Checks if a line starts with '## ' signifying a table title."
  (string-starts-with-p line "## "))

(defun description-line-p (line)
  "Checks if a line is potentially part of a database or table description.
   It should not start with '#' or '|' (or be purely whitespace)."
  (let ((trimmed-line (trim-whitespace line)))
    (and (> (length trimmed-line) 0) ; Not empty or just whitespace
         (not (char= (char trimmed-line 0) #\#))
         (not (char= (char trimmed-line 0) #\|)))))

;;; Data Extractors

(defun extract-database-name (title-line)
  "Extracts the database name from a validated title line.
Assumes line starts with '# '. Trims whitespace from the extracted name."
  (trim-whitespace (subseq title-line 2))) ; Skip "# "

(defun extract-table-name (title-line)
  "Extracts the table name from a validated title line.
Assumes line starts with '## '. Trims whitespace from the extracted name."
  (trim-whitespace (subseq title-line 3))) ; Skip "## "

;;; Main parsing logic (placeholder)

(defun parse-scsd (input)
  "Parses an SCSD input (e.g., stream or string) into an in-memory representation."
  (let* ((lines (read-lines input))
         (current-index 0)
         (db-name nil)
         (db-description nil))
    (declare (ignorable db-name db-description current-index))

    ;; Step 2: Find and process database title
    (let ((title-line-index (position-if #'database-title-line-p lines)))
      (unless title-line-index
        (error 'missing-database-title-error))
      (let* ((title-line (nth title-line-index lines))
             (extracted-name (extract-database-name title-line)))
        (when (string= extracted-name "")
          (error 'malformed-database-title-error
                 :line-number (1+ title-line-index) :title-line title-line))
        (setf db-name extracted-name)
        (setf current-index (1+ title-line-index))))

    ;; Step 3: Collect and join database description lines
    (let ((description-lines
            (loop :for line :in (nthcdr current-index lines)
                  :for line-num :from (1+ current-index) ; 1-based for potential errors
                  :while (and line (description-line-p line))
                  :do (incf current-index) ; Consume the line
                  :collect line)))
      (setf db-description (join-lines description-lines)))

    ;; Step 4: Find and process tables
    (loop :while (< current-index (length lines)) :do
      (let ((line (nth current-index lines))
            (line-num (1+ current-index)))
        (cond
          ((table-title-line-p line)
           (let ((table-name (extract-table-name line)))
             (declare (ignorable table-name))
             (when (string= table-name "")
               (error 'malformed-table-title-error
                      :line-number line-num :title-line line))
             ;; TODO: Process table description, headers, rows...
             (incf current-index)))
          ((string= (trim-whitespace line) "")
           (incf current-index))
          (t
           ;; TODO: Define and signal error
           ;; (warn "Unexpected content found starting line ~A: ~S" line-num line) ; Removed temp warning
           (return))))) ; Stop processing for now

    ;; Placeholder return
    nil))
