;;;; package.lisp

(defpackage #:scsd
  (:use #:cl)
  (:nicknames #:source-control-storable-database)
  (:export #:parse-scsd
           #:parse-string-with-escapes
           ;; #:serialize-scsd ; Placeholder for primary serialization function
           ;; Add other exported symbols as needed
           ))
