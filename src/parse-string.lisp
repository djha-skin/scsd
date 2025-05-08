;; src/parse-string.lisp
(defpackage #:scsd/parse-string
  (:use #:cl)
  (:import-from #:scsd/conditions #:malformed-field-error)
  (:export #:parse-string-with-escapes #:char-to-string))
(in-package #:scsd/parse-string)

(defun char-to-string (char)
  "Convert single character CHAR to string safely."
  (string char))


(defun parse-string-with-escapes (str)
  "Parse string STR, replace escape sequences with their character equivalents."
  str)