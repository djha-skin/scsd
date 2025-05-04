;;;; src/utils.lisp

(defpackage #:scsd/utils
  (:use #:cl)
  (:export #:read-lines
           #:trim-whitespace
           #:string-starts-with-p)) ; Export new utilities

(in-package #:scsd/utils)

(defun read-lines (input)
  "Reads lines from INPUT (pathname, string, or stream) into a list of strings.
Handles CR, LF, and CRLF line endings via READ-LINE, removing the line ending characters."
  (let ((lines (make-array 0 :element-type 'string :adjustable t :fill-pointer 0)))
    (labels ((process-stream (stream)
               (loop :for line := (read-line stream nil nil)
                     :while line
                     :do (vector-push-extend line lines)))
             (get-lines ()
               (coerce lines 'list)))
      (cond
        ((pathnamep input)
         (with-open-file (stream input :direction :input :if-does-not-exist :error)
           (process-stream stream)))
        ((stringp input)
         (with-input-from-string (stream input)
           (process-stream stream)))
        ((streamp input)
         (process-stream input))
        (t (error "Input to read-lines must be a pathname, string, or stream.")))
      (get-lines))))

(defun trim-whitespace (str)
  "Removes leading and trailing whitespace (space, tab, newline, return, linefeed, formfeed) from STR."
  (string-trim '(#\Space #\Tab #\Newline #\Return #\Linefeed #\Page) str)) ; Corrected quote

(defun string-starts-with-p (str prefix)
  "Returns true if STR starts with PREFIX, false otherwise."
  (let ((prefix-len (length prefix))
        (str-len (length str)))
    (and (>= str-len prefix-len)
         (string= str prefix :end1 prefix-len))))
