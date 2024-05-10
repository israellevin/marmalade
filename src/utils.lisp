;;;; General utility functions.

(ql:quickload '("split-sequence" "uiop"))

(defvar *cwd* (uiop/os:getcwd))
(defvar *config-path* (format nil "~A/config.lisp" *cwd*))
(defvar *generators-path* (format nil "~A/generators" *cwd*))

(defun get-config (key &optional (default nil))
  "Returns the value of the specified configuration key."
  (handler-case
    (let* ((config (uiop:read-file-form *config-path*))
           (pair (assoc key config)))
      (if pair
          (cdr pair)
          (if default default (error "Key ~A not found in configuration." key))))
    (error () default)))

(defun split (string &optional (separator " "))
  "Splits a string into a list of substrings."
  (split-sequence:split-sequence separator string))

(provide 'utils)
