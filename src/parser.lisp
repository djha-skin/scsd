;;;; src/parser.lisp

(in-package #:scsd/parser)

;;; Predicates for identifying line types

(defun database-title-line-p (line)
  "Checks if a line starts with '# ' signifying a database title."
  (string-starts-with-p line "# "))

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
         ;; Step 2: Find database title line and its index (line number)
         (title-line-index (position-if #'database-title-line-p lines))
         (title-line (when title-line-index (nth title-line-index lines))))

    ;; Step 3: Validate title line presence
    (unless title-line
      (error 'missing-database-title-error))

    ;; Step 4: Extract and validate name
    (let ((db-name (extract-database-name title-line)))
      (declare (ignorable db-name))
      (when (string= db-name "")
        (error 'malformed-database-title-error
               :line-number (when title-line-index (1+ title-line-index))
               :title-line title-line)))

    ;; Step 5: Process description (Next tasks)
    (let* ((start-index (if title-line-index (1+ title-line-index) 0)) ; Start looking after title
           (description-lines (loop :for line :in (nthcdr start-index lines)
                                     :while (description-line-p line)
                                     :collect line)))
        (declare (ignorable description-lines))
         ;; TODO: Store description
         )

    ;; ... more steps: tables...

    ;; Remove placeholder warning eventually
    (warn "SCSD parsing incomplete.")
    nil)) ; Return nil for now

