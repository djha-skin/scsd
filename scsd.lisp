;;;; scsd.lisp

(in-package #:scsd)

;; This file is loaded after the package definition and the src/ files.
;; It can be used for configuration or, as here, to connect the public API
;; symbols (exported from package.lisp) to their internal implementations.

;; Make the internal parser function the implementation for the public symbol.
;; Using a simple function alias here. More complex scenarios might involve wrappers.
(setf (fdefinition 'parse-scsd) #'scsd/parser:parse-scsd)

;; Add definitions for other exported functions like serialize-scsd here later...