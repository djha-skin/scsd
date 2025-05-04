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
  "Parses an SCSD input (e.g., stream or string) into an in-memory representation."
  ;; TODO: Implement actual parsing logic
  ;; Step 1: Read lines
  (let* ((lines (read-lines input))
         ;; Step 2: Find database title line and its index (line number)
         (title-line-index (position-if #'database-title-line-p lines))
         (title-line (when title-line-index (nth title-line-index lines))))

    ;; Step 3: Validate title line presence
    (unless title-line
      ;; Signal error if no title line found. We don't have an exact line number
      ;; if it's completely missing, maybe signal error without line num? Or search up to first non-comment/blank?
      ;; For now, signal without specific line number if totally missing.
      (error 'missing-database-title-error))

    ;; Step 4: Extract name (if found)
    (let ((db-name (extract-database-name title-line)))
      (declare (ignorable db-name)) ; Mark as used for now
      ;; TODO: Store db-name
      )

    ;; ... more steps: description, tables...

    ;; Remove placeholder warning eventually
    (warn "SCSD parsing incomplete.")
    nil)) ; Return nil for now
