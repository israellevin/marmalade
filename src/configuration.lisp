;;;; Configuration module.

(in-package :marmalade)

(defvar *work-directory* (uiop/os:getcwd))
(defvar *config-file-path* (merge-pathnames #p"config.lisp" *work-directory*))

(defun get-config (key &optional (default nil))
  "Returns the value of the specified configuration key."
  (handler-case
    (let* ((config (uiop:safe-read-file-form *config-file-path*))
           (pair (assoc key config)))
      (if pair
          (cdr pair)
          (if default default (error "Key ~A not found in configuration." key))))
    (error () (error "Valid configuration file ~A not found." *config-file-path*))))

(get-config :privkey-path) (error "Configuration file ~A not found." *config-file-path*)
