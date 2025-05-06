;; src/utils.lisp
(defpackage #:scsd/utils
  (:use #:cl) 
  (:import-from #:scsd/conditions #:malformed-field-error) 
  (:export #:read-lines
           #:trim-whitespace
           #:string-starts-with-p
           #:join-lines
           #:parse-number-string
           #:parse-unicode-escape
           #:hex-digit-p
           #:hex-digit-to-int))
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

(defun hex-digit-p (c)
  (or (and (char>= c #\0) (char<= c #\9))
      (and (char>= c #\A) (char<= c #\F))
      (and (char>= c #\a) (char<= c #\f))))

(defun hex-digit-to-int (c)
  (cond
    ((and (char>= c #\0) (char<= c #\9)) (- (char-code c) (char-code #\0)))
    ((and (char>= c #\A) (char<= c #\F)) (+ (- (char-code c) (char-code #\A)) 10))
    ((and (char>= c #\a) (char<= c #\f)) (+ (- (char-code c) (char-code #\a)) 10))
    (t (error "Invalid hex digit: ~C" c))))

(defun parse-unicode-escape (str start length)
  "Parse a unicode escape sequence \uXXXX (length 4) or \UXXXXXXXX (length 8).
   Returns the character and new index after the escape sequence."
  (if (<= (+ start length) (length str))
      (let ((value 0))
        (loop for pos from start below (+ start length)
              do (setf value (+ (* value 16) (hex-digit-to-int (elt str pos)))))
        (values (code-char value) (+ start length)))
      (error "Malformed escape: truncated hex sequence in ~A" str)))

;;; NOTE: parse-string-with-escapes removed and moved to src/parse-string.lisp

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
        (declare (ignore position)) ; <-- Ignore the second value
        (if (numberp value)
            value
            (signal 'malformed-field-error 
                    :line-number line-number
                    :field-content field-str
                    :expected-type "Number"
                    :reason (format nil "Parsed value '~A' is not a number." value)))))))
