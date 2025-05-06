;; src/parse-string.lisp
(defpackage #:scsd/parse-string
  (:use #:cl)
  (:import-from #:scsd/conditions #:malformed-field-error)
  (:export #:parse-string-with-escapes #:char-to-string))
(in-package #:scsd/parse-string)

(defun char-to-string (char)
  "Convert single character CHAR to string safely."
  (string char))

(defun process-escape-sequence (str i)
  "Process an escape sequence starting at index I in string STR.\nReturns two values: the string result for the escape, and the new index after escape." 
   (if  (< (+ i 1) (length str))
        (let ((next-char (elt str (+ i 1))))
             (case next-char
               (#\\  (values "\\" (+ i 2)))
               (#\" (values "\"" (+ i 2)))
               (#\n (values  "\n" (+ i 2)))
               (#\t (values "\t" (+ i 2)))
               (otherwise  (values (concatenate 'string "\\" (char-to-string next-char)) (+ i 2)))))
       (error \"Incomplete escape sequence at end of string.\")))


(defun parse-string-with-escapes (str)
  "Parse string STR, replace escape sequences with their character equivalents."
  (let ((accumulator "")
        (i 0)
        (len (length str)))
    (loop while (< i len)
          for ch = (elt str i)
          do (if (char= ch #\\)
                 (multiple-value-bind (res new-i) (process-escape-sequence str i)
                   (setf accumulator (concatenate 'string accumulator res)
                         i new-i))
                 (progn
                   (setf accumulator (concatenate 'string accumulator (char-to-string ch))
                         i (1+ i))))
          finally (return accumulator))))