(defpackage #:scsd
  (:use #:cl)
  (:import-from #:alexandria
                #:when-let
                #:if-let)
  (:export #:parse-scsd
           #:parse-table
           #:*scsd-database*
           #:scsd-error
           #:scsd-parse-error))