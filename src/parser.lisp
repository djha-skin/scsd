(defpackage #:scsd/parser
  (:use #:cl #:scsd/parse-string #:scsd/utils #:scsd/conditions #:str)
  (:import-from #:alexandria #:if-let #:ends-with-subseq)
  (:import-from #:scsd/utils #:parse-number-string)
  (:export #:parse-scsd
           #:database-title-line-p #:extract-database-name
           #:description-line-p
           #:table-title-line-p #:extract-table-name
           #:pipe-table-line-p #:split-pipe-table-line
           #:parse-field))
(in-package #:scsd/parser)


(defun parse-scsd (pathname)
  (declare (ignore pathname))
  "Dummy Parse"
  
  ;;(error \"TODO\"))
  nil)