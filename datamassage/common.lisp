(format t "loading libraries...~%")
(require 'cl-ppcre)
(require 'alexandria)
(require 'cl-fad)

(defmacro get-or-init (place init-expr)
  "get or initialize idiom"
  `(or ,place (setf ,place ,init-expr)))

(defun format-tab-delimited-list (s list)
  (loop for (i next) on list
        do (princ i s)
        if next 
        do (princ #\Tab s)
        else do (princ #\Newline s)))

(format t "Loading and running code...~%")
(terpri)