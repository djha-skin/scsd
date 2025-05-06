(defpackage #:scsd/parser
  (:use #:cl #:scsd/utils #:scsd/conditions #:str)
  (:import-from #:alexandria #:if-let #:ends-with-subseq)
  (:import-from #:scsd/utils #:parse-number-string)
  (:export #:parse-scsd
           #:database-title-line-p #:extract-database-name
           #:description-line-p
           #:table-title-line-p #:extract-table-name
           #:pipe-table-line-p #:split-pipe-table-line
           #:parse-field))
(in-package #:scsd/parser)

(defun database-title-line-p (line)
  (and line (string-starts-with-p line "# ")))

(defun extract-database-name (line)
  (when (database-title-line-p line)
    (trim-whitespace (subseq line 2))))

(defun description-line-p (line)
  (and line
       (not (uiop:emptyp line))
       (not (string-starts-with-p line "#"))
       (not (string-starts-with-p line "|"))))

(defun table-title-line-p (line)
  (and line (string-starts-with-p line "## ")))

(defun extract-table-name (line)
  (when (table-title-line-p line)
    (trim-whitespace (subseq line 3))))

(defun pipe-table-line-p (line)
  (and line
       (> (length line) 1)
       (string-starts-with-p line "|")
       (ends-with-subseq "|" line)))

(defun split-pipe-table-line (line)
  "Splits a pipe table line (e.g., | Col1 | Col2 |) into a list of strings (" Col1 " " Col2 ").
   Handles empty columns and preserves internal whitespace.
   Returns NIL if the line doesn't start and end with '|'."
  (when (pipe-table-line-p line)
    (let ((trimmed-line (subseq line 1 (1- (length line)))))
      (str:split #\| trimmed-line :omit-nulls nil))))

(defun parse-field (field-str type-marker &optional line-number)
  "Parses a raw string field based on the type marker. Returns parsed value or signals error."
  (let ((trimmed-field (trim-whitespace field-str)))
    (cond
      ;; String: Return the original string, preserving whitespace
      ((string= type-marker "-") field-str)
      ;; Keyword/Symbol: Intern as keyword, handle empty string as NIL
      ((string= type-marker ":-")
       (if (uiop:emptyp trimmed-field)
           nil
           (intern (string-upcase trimmed-field) :keyword)))
      ;; Number: Use imported parse-number-string, handle empty string as NIL
      ((string= type-marker "-:")
       (if (uiop:emptyp trimmed-field)
           nil
           (parse-number-string trimmed-field line-number)))
      ;; Boolean: Check for 'true'/'false' (case-insensitive), handle empty as NIL
      ((string= type-marker ":-:")
       (cond
         ((uiop:emptyp trimmed-field) nil)
         ((string-equal trimmed-field "true") t)
         ((string-equal trimmed-field "false") nil)
         (t (signal 'malformed-field-error
                    :line-number line-number
                    :field-content field-str
                    :expected-type "Boolean ('true' or 'false')"
                    :reason (format nil "Invalid boolean value '~A'." field-str)))))
      ;; Unknown type marker
      (t (error "Internal error: Unknown type marker '~A' encountered during field parsing." type-marker)))))

;;; Main parser function
(defun parse-scsd (pathname)
  "Parses an SCSD file into an in-memory representation (currently returns parsed rows for the first table)."
  (let* ((lines (read-lines pathname))
         (line-count (length lines))
         (current-index 0)
         (db-name nil)
         (db-description-lines '())
         (parsed-rows '())) ; Temporary storage for first table's rows

    ;; Find Database Name (H1)
    (if-let (line (and (< current-index line-count) (nth current-index lines)))
      (if (database-title-line-p line)
        (progn
          (setf db-name (extract-database-name line))
          (unless (and db-name (not (uiop:emptyp db-name)))
            (signal 'malformed-database-title-error :line-number (1+ current-index)))
          (incf current-index))
        (signal 'missing-database-title-error :line-number 1))
      (signal 'missing-database-title-error :line-number 1)) ; Error if file is empty or first line missing

    ;; Collect Database Description
    (loop while (< current-index line-count)
          for line = (nth current-index lines)
          while (description-line-p line)
          do (push line db-description-lines)
             (incf current-index))
    (let ((db-description (join-lines (nreverse db-description-lines))))
      (declare (ignorable db-description)))

    ;; Process Tables
    (loop while (< current-index line-count)
          for line = (nth current-index lines)
          for current-line-number = (1+ current-index)
          do
       (cond
         ;; Skip blank lines between elements
         ((uiop:emptyp (trim-whitespace line))
          (incf current-index))

         ;; Detect Table Title (H2)
         ((table-title-line-p line)
          (let* ((table-name (extract-table-name line))
                 (table-description-lines '())
                 (column-names nil)
                 (column-types nil)
                 (current-table-rows '()))

            ;; Validate Table Name
            (unless (and table-name (not (uiop:emptyp table-name)))
              (signal 'malformed-table-title-error :line-number current-line-number))
            (incf current-index)

            ;; Collect Table Description
            (loop while (< current-index line-count)
                  for desc-line = (nth current-index lines)
                  while (description-line-p desc-line)
                  do (push desc-line table-description-lines)
                     (incf current-index))
            (let ((table-description (join-lines (nreverse table-description-lines))))
              (declare (ignorable table-description)))

            ;; Find Header Line
            (let ((header-line-index current-index))
              (if-let (header-line (and (< current-index line-count) (nth current-index lines)))
                (if (pipe-table-line-p header-line)
                  (progn
                    (setf column-names (split-pipe-table-line header-line))
                    (incf current-index))
                  (signal 'missing-header-error :line-number (1+ header-line-index) :table-name table-name))
                (signal 'missing-header-error :line-number (1+ header-line-index) :table-name table-name)))

            ;; Find Type Specification Line
            (let ((typespec-line-index current-index))
              (if-let (typespec-line (and (< current-index line-count) (nth current-index lines)))
                (if (pipe-table-line-p typespec-line)
                  (let ((raw-types (split-pipe-table-line typespec-line)))
                    (unless (= (length raw-types) (length column-names))
                      (signal 'mismatched-typespec-error
                              :line-number (1+ typespec-line-index)
                              :header-count (length column-names)
                              :typespec-count (length raw-types)))
                    ;; Validate and store trimmed types
                    (setf column-types
                          (loop for type-str in raw-types
                                for col-idx from 0
                                for trimmed = (trim-whitespace type-str)
                                unless (member trimmed '("-" ":-" "-:" ":-:") :test #'string=)
                                  do (signal 'malformed-typespec-error
                                             :line-number (1+ typespec-line-index)
                                             :column-index col-idx
                                             :type-value type-str)
                                collect trimmed))
                    (incf current-index))
                  (signal 'missing-typespec-error :line-number (1+ typespec-line-index) :table-name table-name))
                (signal 'missing-typespec-error :line-number (1+ typespec-line-index) :table-name table-name)))

            ;; Process data rows
            (loop while (< current-index line-count)
                  for row-line-index = current-index
                  for row-line = (nth current-index lines)
                  while (pipe-table-line-p row-line)
                  do
               (let ((fields (split-pipe-table-line row-line)))
                 (unless (= (length fields) (length column-names))
                   (signal 'mismatched-field-count-error
                           :line-number (1+ row-line-index)
                           :header-count (length column-names)
                           :field-count (length fields)))
                 ;; Parse each field based on column type
                 (push (loop for field-str in fields
                             for type-marker in column-types
                             collect (parse-field field-str type-marker (1+ row-line-index)))
                       current-table-rows)
                 (incf current-index)))

            ;; TEMPORARY: Assign reversed rows to outer variable for testing return
            (setf parsed-rows (nreverse current-table-rows))
            ;; TEMPORARY: Stop after the first table for testing
            (return-from parse-scsd parsed-rows))
          ) ; End inner let* for table processing

         ;; Found something unexpected
         (t
          (signal 'scsd-parse-error :line-number current-line-number
                  :reason (format nil "Unexpected content: '~A'" line))
          (incf current-index)) 
         )
       ) ; End cond
    ) ; End main table loop

    ;; This is unreachable due to the return-from above, causing warning
    ;; parsed-rows 
  ) ; End main let*
)