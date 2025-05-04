;;;; src/parser.lisp

(in-package #:scsd/parser)

;;; Predicates for identifying line types

(defun database-title-line-p (line)
  "Checks if a line starts with '# ' signifying a database title."
  (string-starts-with-p line "# "))

;;; Data Extractors

(defun extract-database-name (title-line)
  "Extracts the database name from a validated title line.
Assumes line starts with '# '. Trims whitespace from the extracted name."
  (trim-whitespace (subseq title-line 2))) ; Skip "# "

;;; Main parsing logic (placeholder)

(defun parse-scsd (input)
  "Parses an SCSD input (e.g., stream or string) into an in-memory representation.
  Placeholder implementation."
  ;; TODO: Implement actual parsing logic
  ;; Step 1: Read lines
  (let ((lines (read-lines input)))
    ;; Step 2: Find database title
    (let ((title-line (find-if #'database-title-line-p lines)))
      (when title-line
        (let ((db-name (extract-database-name title-line)))
          (declare (ignorable db-name)) ; Mark as used for now
          ;; TODO: Store db-name
          )))
    ;; ... more steps
    )
  (warn "SCSD parsing not yet implemented.")
  nil) ; Return nil for now
