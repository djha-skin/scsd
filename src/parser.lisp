(defpackage #:scsd/parser
  (:use #:cl #:scsd/utils #:scsd/conditions #:str)
  (:import-from #:alexandria #:if-let #:ends-with-subseq)
  (:import-from #:scsd/utils
                #:parse-number-string
                #:unescape-string
                #:string-contains-unescaped-p)
  (:export #:parse-scsd
           #:database-title-line-p #:extract-database-name
           #:description-line-p
           #:table-title-line-p #:extract-table-name
           #:pipe-table-line-p #:split-pipe-table-line
           #:parse-field))

(in-package #:scsd/parser)

[Keep previous functions unchanged until parse-field]

(defun parse-field (field-str type-marker &optional line-number)
  "Parses a raw string field based on the type marker. Returns parsed value or signals error."
  (declare (type string field-str)
           (type string type-marker)
           (type (or null fixnum) line-number))
  (let* ((trimmed-field (trim-whitespace field-str))
         ;; Handle escapes early for any field-str that needs it
         (unescaped-field (if (find #\\ field-str)
                             (unescape-string field-str line-number)
                             field-str)))
    (cond
      ;; String: Return the original string with escapes processed
      ((string= type-marker "-") unescaped-field)
      
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
      (t (error "Internal error: Unknown type marker '~A' encountered during field parsing."
                type-marker)))))