;;;; Common functions.

(in-package :marmalade)

(defvar *work-directory* (uiop/os:getcwd))
(defvar *config-file-path* (format nil "~Aconfig.lisp" *work-directory*))

(defun get-config (key &optional (default nil))
  "Returns the value of the specified configuration key."
  (handler-case
    (let* ((config (uiop:safe-read-file-form *config-file-path*))
           (pair (assoc key config)))
      (if pair
          (cdr pair)
          (if default default (error "Key ~A not found in configuration." key))))
    (error () (error "Valid configuration file ~A not found." *config-file-path*))))

(defun directory-map (file-function path)
  "Returns a map of file-function over the files in path, in reverse chronological order, ignoring nils."
  (remove-if #'null
             (mapcar
               (lambda (file-path)
                 (let*
                   ((file-name (file-namestring file-path))
                    (file-name-parts (split-sequence:split-sequence #\. file-name))
                    (file-basename (first file-name-parts))
                    (file-extension (second file-name-parts)))
                   (funcall file-function `(:path ,file-path :basename ,file-basename :extension ,file-extension))))
               (sort (uiop:directory-files path) #'> :key #'file-write-date))))
