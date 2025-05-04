;;;; src/package-parser.lisp

(defpackage #:scsd/parser
  (:use #:cl #:scsd/utils #:scsd/conditions) ; Added conditions package
  (:export #:parse-scsd
           ))
