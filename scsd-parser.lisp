(in-package :scsd)

(defun trim-string (str)
  "Trim whitespace from both ends of STR."
  (string-trim '(#\Space #\Tab #\Newline) str))

(defun parse-scsd-file (file-path)
  "Parse the SCSD file at FILE-PATH."
  (with-open-file (in file-path)
    (let ((database-title nil)
          (database-description nil)
          (tables (make-hash-table :test 'equal))
          (current-table nil))
      (loop for line = (read-line in nil)
            while line
            do (cond
                 ((string-prefix-p "# " line)
                  (setf database-title (trim-string (subseq line 2)))
                  (setf database-description ""))
                 ((string-length line)
                  (cond 
                    ((string-prefix-p "## " line)
                     (setf current-table (trim-string (subseq line 3)))
                     (setf (gethash current-table tables) 
                           (list :description "" :columns nil :types nil :records '())))
                    ((string-prefix-p "|" line)
                     (let ((table (gethash current-table tables)))
                       (cond
                         ((null (getf table :columns))
                          (setf (getf table :columns) 
                                (mapcar #'trim-string (subseq line 1 (- (length line) 1)) (split-sequence #\| line))))
                         ((null (getf table :types))
                          (setf (getf table :types) 
                                (mapcar #'trim-string (subseq line 1 (- (length line) 1)) (split-sequence #\| line))))
                         (t
                          (push (mapcar #'trim-string (subseq line 1 (- (length line) 1)) (split-sequence #\| line)) 
                                (getf table :records))))))
                    (t
                     (if current-table
                         (setf (getf (gethash current-table tables) :description)
                               (concatenate 'string (getf (gethash current-table tables) :description) line " "))
                         (setf database-description
                               (concatenate 'string database-description line " ")))))))
      (list :database-title database-title
            :database-description database-description
            :tables tables))))

;;; Utility function for split
(defun split-sequence (delimiter sequence &key (start 0) (end (length sequence)))
  (let ((start (position delimiter sequence :start start :end end))
        (results '()))
    (loop
      for pos = (position delimiter sequence :start start :end end)
      while pos
      do (push (subseq sequence start pos) results)
         (setf start (1+ pos)))
    (reverse results)))