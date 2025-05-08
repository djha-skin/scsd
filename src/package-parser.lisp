;;;; src/package-parser.lisp

(defpackage #:scsd/parser
  (:use #:cl #:scsd/utils #:scsd/conditions)
  (:import-from #:str #:split)
  (:import-from #:alexandria #:if-let #:ends-with-subseq)
  (:export #:parse-scsd
           #:database-title-line-p 
           #:extract-database-name
           #:description-line-p
           #:table-title-line-p 
           #:extract-table-name
           #:pipe-table-line-p
           #:split-pipe-table-line
           #:parse-field))