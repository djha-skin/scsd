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

;;; Main parsing logic (placeholder)

(defun parse-scsd (input)
  "Parses an SCSD input (e.g., stream or string) into an in-memory representation."
  ;; TODO: Implement actual parsing logic
  ;; Step 1: Read lines
  (let* ((lines (read-lines input))
         (current-index 0)
         (db-name nil)
         (db-description-lines nil))
    (declare (ignorable db-name db-description-lines current-index)) ; Declare at top of LET* body

    ;; Step 2: Find and process database title
    (let ((title-line-index (position-if #'database-title-line-p lines)))
      (unless title-line-index
        (error 'missing-database-title-error)) ; Must have title

      (let* ((title-line (nth title-line-index lines))
             (extracted-name (extract-database-name title-line)))
        (when (string= extracted-name "")
          (error 'malformed-database-title-error
                 :line-number (1+ title-line-index)
                 :title-line title-line))
        (setf db-name extracted-name)
        (setf current-index (1+ title-line-index)))) ; Advance index past title

    ;; Step 3: Collect database description lines
    (setf db-description-lines
          (loop :for line :in (nthcdr current-index lines)
                :for line-num :from (1+ current-index) ; 1-based for potential errors
                :while (and line (description-line-p line)) ; Stop if not description line or EOF
                :do (incf current-index) ; Consume the line
                :collect line))

    ;; ... more steps ...

    (warn "SCSD parsing incomplete.")
    nil))

