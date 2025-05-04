;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: SCSD.LOW-LEVEL; Base: 10 -*-
;;; vim: set filetype=lisp:

;;; Define the low-level SCSD parser using esrap.

(in-package :scsd.low-level)

;;;---------------------------------------------------------------------------
;;; Esrap Parser Definition
;;;---------------------------------------------------------------------------

(defrule ws* (* (or #\Space #\Tab #\Newline #\Return))
  (:constant nil)) ; Ignore whitespace in output

(defrule newline (or (text #\Newline) (text #.(format nil "~C~C" #\Return #\Newline)))
  (:text t))

(defrule pipe (and ws* #\| ws*)
  ;; ABNF has this consuming surrounding whitespace, return nil
  (:constant nil))

;;; ABNF VCHAR = %x21-7E (visible characters)
(defun vchar-p (char)
  (<= #x21 (char-code char) #x7E))

;;; Value character for header items (Any VCHAR)
(defrule valuechar vchar-p ; Use the predicate function name directly
  (:text t))

;;; Key character for header items (ALPHA / DIGIT / "_" / "-")
;;; Replace (member ...) with direct characters in the or
(defrule keychar (or alphanumericp #\_ #\-)
  (:text t))

;;; Table name character (ALPHA / DIGIT / "_" / "-")
;;; Replace (member ...) with direct characters in the or
(defrule tablename-char (or alphanumericp #\_ #\-)
  (:text t))

(defrule tablename (+ tablename-char)
  (:text t))

;;; Character allowed within table cells/columns (anything except pipe and newline/cr)
(defrule cell-char (not (or #\| #\Newline #\Return))
  (:text t))

;;; Content of a table cell, trimmed. ABNF: *ws *(VCHAR / WSP) *ws
;;; The (* cell-char) captures *(VCHAR / WSP) part reasonably well, 
;;; excluding pipe/newline. Trimming is handled by :lambda.
(defrule cell-content (* cell-char) 
  (:lambda (list)
    (string-trim '(#\Space #\Tab) (text list))))

;;; Name of a table column, trimmed. ABNF: *ws 1*(VCHAR / WSP) *ws
;;; Need to ensure the result isn't empty *after* trimming? ABNF 1* implies non-empty content.
;;; Let's keep it simple for now and trim. Validation might be needed later.
(defrule column-name (* cell-char) 
  (:lambda (list)
    (string-trim '(#\Space #\Tab) (text list))))

;;; ABNF: pipe *(1*cell-content pipe) *newline
;;; Requires at least zero columns according to * rule. But 1*cell-content implies non-empty cell.
;;; Esrap rule: (+ (and cell-content pipe)) requires at least one column.
;;; Let's use * for zero or more columns, matching ABNF structure better.
;;; We'll rely on cell-content potentially being empty string after trim.
(defrule table-row (and pipe (* (and cell-content pipe)) ws* newline) 
  (:destructure (p1 cells-with-pipes &rest rest)
    (declare (ignore p1 rest))
    ;; cells-with-pipes is like '((cell1 pipe) (cell2 pipe) ...)
    `(:row ,(mapcar #'first cells-with-pipes))))

;;; ABNF: pipe *(1*column-name pipe) *newline
;;; Use * for zero or more columns.
(defrule table-header (and pipe (* (and column-name pipe)) ws* newline) 
  (:destructure (p1 columns-with-pipes &rest rest)
    (declare (ignore p1 rest))
    ;; columns-with-pipes is like '((col1 pipe) (col2 pipe) ...)
    `(:header ,(mapcar #'first columns-with-pipes))))

;;; ABNF: pipe *("-" pipe) *newline. Requires zero or more separators.
(defrule table-separator (and pipe (* (and (+ #\-) pipe)) ws* newline) 
  (:destructure (p1 separators-with-pipes &rest rest)
    (declare (ignore p1 separators-with-pipes rest))
    :separator))

;;; Optional caption for a table
;;; ABNF: %s"caption:" *ws 1*VCHAR *ws *newline
(defrule caption (and "caption:" ws* (+ (not (or #\Newline #\Return))) ws* newline)
  (:destructure (cap ws1 text ws2 nl)
    (declare (ignore cap ws1 ws2 nl))
    `(:caption ,(string-trim '(#\Space #\Tab) (text text))))) ; Trim caption text

;;; Starting tag of a table
;;; ABNF: %s"[table" *ws 1*tablename *ws %s"]" *newline
;;; Using ws+ after "[table" for robustness.
(defrule table-start (and "[table" ws+ tablename ws* "]" ws* newline) 
  (:destructure (ts ws1 name ws2 te ws3 nl)
    (declare (ignore ts ws1 ws2 te ws3 nl))
    `(:table-start ,name)))

;;; Ending tag of a table
;;; ABNF: %s"[/table]" *newline
(defrule table-end (and "[/table]" ws* newline) 
  (:constant :table-end))

;;; A complete table structure
;;; ABNF: table-start *ws [caption] *ws table-header *ws table-separator *ws *table-row *ws table-end
(defrule table (and table-start ws* (? caption) ws* table-header ws* table-separator ws* (* table-row) ws* table-end)
  (:destructure (start ws1 cap ws2 header ws3 sep ws4 rows ws5 end)
    (declare (ignore ws1 ws2 ws3 sep ws4 ws5 end)) ; Ignore separator symbol :separator
    `(:table ,(second start) ; Table name
             ,@(when cap (list cap)) ; Optional caption
             ,header ; Header structure (:header (...))
             ,rows))) ; List of row structures '((:row (...)) (:row (...)))

;;; Starting tag of the header section
;;; ABNF: %s"[header]" *newline
(defrule header-start (and "[header]" ws* newline)
  (:constant :header-start))

;;; Ending tag of the header section
;;; ABNF: %s"[/header]" *newline
(defrule header-end (and "[/header]" ws* newline)
  (:constant :header-end))

;;; A key-value item in the header section
;;; ABNF: 1*keychar *ws %s":" *ws 1*valuechar *ws *newline
(defrule header-item (and (+ keychar) ws* ":" ws* (+ valuechar) ws* newline) ; Require newline terminator
  (:destructure (key kws col vws value vws2 nl)
    (declare (ignore kws col vws vws2 nl))
    `(,(text key) ,(text value)))) ; Return as (key value) list

;;; The complete header section
;;; ABNF: header-start *ws *header-item *ws header-end
(defrule header (and header-start ws* (* header-item) ws* header-end)
  (:destructure (start ws1 items ws2 end)
    (declare (ignore start ws1 ws2 end))
    `(:header ,items)))

;;; The entire SCSD document
;;; ABNF: *ws [header] *ws *(table *ws) *ws
(defrule document (and ws* (? header) ws* (* (and table ws*)) ws*)
  (:destructure (ws1 head ws2 tables-ws ws3)
    (declare (ignore ws1 ws2 ws3))
    `(:document ,@(when head (list head)) ; Optional header
                ,(mapcar #'first tables-ws)))) ; List of tables

;;;---------------------------------------------------------------------------
;;; Public Parsing Function
;;;--------------------------------------------------------------------------- 

(defun parse-scsd (input)
  "Parses the given string INPUT as an SCSD document.
  Returns the parsed structure or signals an ESRAP:ESRAP-PARSE-ERROR on failure."
  (esrap:parse 'document input))
