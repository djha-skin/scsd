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

(defun pipe-table-line-p (line)
  "Checks if a line starts and ends with a pipe ('|').
   This could be a header, type spec, or data row."
  (let ((len (length line)))
    (and (>= len 2) ; Must have at least start and end pipe
         (char= (char line 0) #\|)
         (char= (char line (1- len)) #\|))))

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
           (incf current-index) ; Consume title line
           (let* ((table-name (extract-table-name line))
                  (table-description nil))
             (declare (ignorable table-name table-description))
             ;; Validate table name
             (when (string= table-name "")
               (error 'malformed-table-title-error
                      :line-number line-num :title-line line))

             ;; Collect table description
             (let ((description-lines
                     (loop :for desc-line := (when (< current-index (length lines)) (nth current-index lines))
                           :while (and desc-line (description-line-p desc-line))
                           :do (incf current-index)
                           :collect desc-line)))
               (setf table-description (join-lines description-lines)))

             ;; Expect Header line
             (let ((header-line (when (< current-index (length lines)) (nth current-index lines))))
               (declare (ignorable header-line))
               (unless (and header-line (pipe-table-line-p header-line))
                 ;; TODO: Define and signal missing-header-error
                 (error "Missing or invalid table header line after description for table '~A' near line ~A"
                        table-name (1+ current-index)))
               (incf current-index) ; Consume header line
               ;; TODO: Split header line (next task)
               )

             ;; TODO: Process types, rows (starting at current-index)
             )) ; End LET* for current table
          ((string= (trim-whitespace line) "")
           (incf current-index)) ; Skip blank lines between elements
          (t
           ;; Found unexpected content
           ;; TODO: Define and signal error
           (warn "Unexpected content found starting line ~A: ~S" line-num line)
           (return))))) ; Stop processing for now

    ;; Placeholder return
    nil))
