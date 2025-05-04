;;; -*- Mode: LISP; Syntax: COMMON-LISP; Base: 10 -*-
;;; vim: set filetype=lisp:

;;; Define the main package for the SCSD low-level parser/implementation details.

(defpackage :scsd.low-level
  (:use #:cl #:esrap)
  (:export #:parse-scsd))
