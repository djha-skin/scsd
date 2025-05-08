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
  (let ((result (make-string 0 :adjustable t :fill-pointer 0)))
    (loop with i = 0
          while (< i (length str))
          do (let ((c (char str i)))
               (case c
                 (#\\ (incf i)
                       (if (< i (length str))
                           (let ((next-char (char str i)))
                             (case next-char
                               (#\u (incf i)
                                     (multiple-value-bind (unicode-char new-index)
                                         (scsd/utils:parse-unicode-escape str i 4)
                                       (vector-push-extend unicode-char result)
                                       (setf i new-index)))
                               (#\U (incf i)
                                     (multiple-value-bind (unicode-char new-index)
                                         (scsd/utils:parse-unicode-escape str i 8)
                                       (vector-push-extend unicode-char result)
                                       (setf i new-index)))
                               (t (vector-push-extend next-char result))))
                           (error "Malformed escape sequence at end of string.")))
                 (t (vector-push-extend c result))
               )
               (incf i)))
    (copy-seq result)))