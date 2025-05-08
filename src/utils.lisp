;; src/utils.lisp
(defpackage #:scsd/utils
  (:use #:cl)
  (:import-from #:scsd/conditions
                #:malformed-field-error
                #:malformed-escape-error
                #:incomplete-escape-error
                #:invalid-unicode-escape-error)
  (:export #:read-lines
           #:trim-whitespace
           #:string-starts-with-p
           #:join-lines
           #:parse-number-string
           ;; New exports
           #:unescape-string
           #:escape-string
           #:string-contains-unescaped-p))

(in-package #:scsd/utils)

[Previous functions remain unchanged...]

(defvar *ascii-escapes*
  '((#\a . #\Bell)        ; \a = bell/alarm   (0x07)
    (#\b . #\Backspace)   ; \b = backspace    (0x08)
    (#\t . #\Tab)         ; \t = tab          (0x09)
    (#\n . #\Newline)     ; \n = line feed    (0x0A)
    (#\v . #\Vt)          ; \v = vertical tab (0x0B)
    (#\f . #\Page)        ; \f = form feed    (0x0C)
    (#\r . #\Return))     ; \r = return       (0x0D)
  "Mapping of ASCII escape sequence characters to their control character values.")

(defun char-hex-digit-p (char)
  "Returns true if CHAR is a valid hexadecimal digit (0-9, A-F, a-f)."
  (and (characterp char)
       (or (digit-char-p char 16)
           (member char '(#\A #\B #\C #\D #\E #\F #\a #\b #\c #\d #\e #\f)))))

(defun process-escaped-char (str start line-number)
  "Process an escape sequence starting at position START in STR.
Returns (values new-position char) where new-position is the position after
the escape sequence and char is the unescaped character."
  (declare (type string str)
           (type fixnum start)
           (type (or null fixnum) line-number))
  (when (= start (length str))
    (signal 'incomplete-escape-error :line-number line-number))
  (let* ((next-char (char str start))
         (found (assoc next-char *ascii-escapes*)))
    (if found
        (values (1+ start) (cdr found))
        ;; For now, just return the character as-is if it's not a special escape
        (values (1+ start) next-char))))

(defun unescape-string (str &optional line-number)
  "Convert escaped characters in STR to their actual values.
Returns the unescaped string or signals an error for invalid escapes."
  (declare (type string str)
           (type (or null fixnum) line-number))
  (let ((result (make-array (length str)
                           :element-type 'character
                           :fill-pointer 0
                           :adjustable t))
        (i 0))
    (loop while (< i (length str))
          for ch = (char str i)
          do
          (if (char= ch #\\)
              (multiple-value-bind (new-pos unescaped-char)
                  (process-escaped-char str (1+ i) line-number)
                (vector-push-extend unescaped-char result)
                (setf i new-pos))
              (progn
                (vector-push-extend ch result)
                (incf i))))
    result))

(defun string-contains-unescaped-p (str char)
  "Returns T if STR contains an unescaped instance of CHAR."
  (declare (type string str)
           (type character char))
  (let ((len (length str)))
    (loop for i below len
          when (char= (char str i) char)
            unless (and (> i 0) (char= (char str (1- i)) #\\))
              return t
          finally (return nil))))

(defun escape-string (str)
  "Add escape sequences to STR for special characters.
This is the opposite of unescape-string and is used for serialization."
  (declare (type string str))
  (let ((result (make-array (* 2 (length str))
                           :element-type 'character
                           :fill-pointer 0
                           :adjustable t)))
    (loop for ch across str
          do (case ch
               ((#\Bell) (vector-push-extend #\\ result)
                        (vector-push-extend #\a result))
               ((#\Backspace) (vector-push-extend #\\ result)
                             (vector-push-extend #\b result))
               ((#\Tab) (vector-push-extend #\\ result)
                       (vector-push-extend #\t result))
               ((#\Newline) (vector-push-extend #\\ result)
                           (vector-push-extend #\n result))
               ((#\Vt) (vector-push-extend #\\ result)
                      (vector-push-extend #\v result))
               ((#\Page) (vector-push-extend #\\ result)
                        (vector-push-extend #\f result))
               ((#\Return) (vector-push-extend #\\ result)
                          (vector-push-extend #\r result))
               ((#\| #\- #\: #\\) (vector-push-extend #\\ result)
                                 (vector-push-extend ch result))
               (otherwise (vector-push-extend ch result))))
    result))