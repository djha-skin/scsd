;; src/utils.lisp
(defpackage #:scsd/utils
  (:use #:cl) 
  (:import-from #:scsd/conditions #:malformed-field-error) 
  (:export #:read-lines
           #:trim-whitespace
           #:string-starts-with-p
           #:join-lines
           #:parse-number-string))
(in-package #:scsd/utils)

(defun read-lines (pathname)
  "Reads all lines from a file specified by PATHNAME into a list of strings."
  (uiop:read-file-lines pathname))

(defun trim-whitespace (str)
  "Removes leading and trailing whitespace from STR."
  (string-trim '(#\Space #\Tab #\Newline #\Return #\Linefeed #\Page) str))

(defun string-starts-with-p (str prefix)
  "Returns T if STR starts with PREFIX, NIL otherwise."
  (and (>= (length str) (length prefix))
       (string= prefix str :end2 (length prefix))))

(defun join-lines (lines)
  "Joins a list of strings LINES with newline characters."
  (format nil "~{~A~^~%~}" lines))


;; Moved from parser.lisp
(defun parse-number-string (field-str &optional line-number)
  (declare (ignorable line-number))
  (unless (uiop:emptyp field-str)
    (handler-case (read-from-string field-str)
      (reader-error (c)
        (signal 'malformed-field-error 
                :line-number line-number
                :field-content field-str
                :expected-type "Number"
                :reason (format nil "Reader error: ~A" c)))
      (:no-error (value &optional position) ; <-- Changed lambda list here
        (declare (ignore position))         ; <-- Ignore the second value
        (if (numberp value)
            value
            (signal 'malformed-field-error 
                    :line-number line-number
                    :field-content field-str
                    :expected-type "Number"
                    :reason (format nil "Parsed value '~A' is not a number." value)))))))
