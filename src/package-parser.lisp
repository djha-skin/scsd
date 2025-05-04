;;;; src/package-parser.lisp

(defpackage #:scsd/parser
  (:use #:cl #:scsd/utils #:scsd/conditions)
  (:import-from #:str #:split) ; <<<<< IMPORT SPLIT
  (:export #:parse-scsd
           ))
