;;;; Jam related functions.

(in-package :marmalade)

(defvar *current-jam* nil)

(defun make-jam-log (jam-directory)
  (org.shirakumo.verbose:restart-global-controller)
  (org.shirakumo.verbose:define-pipe
    () (org.shirakumo.verbose:file-faucet :file (merge-pathnames #P"jam.log" jam-directory))))

(defun jam-log (message &optional (level :info))
  (org.shirakumo.verbose:log level :jam message))

(defun jam-connect (jam-name)
  "Connects to a (potentially new) jam with the specified name."
  (let ((jam-path (merge-pathnames (format nil "~A/" jam-name) *jams-directory*)))
    (ensure-directories-exist jam-path)
    (make-jam-log jam-path)
    (jam-log (format nil "Starting jam ~A." jam-name))
    (uiop:launch-program (format nil "~A --dir ~A --save '' --appendonly yes --appenddirname redis"
                                 (get-config :redis-command) jam-path))
    (setf *current-jam* jam-name))
  (push #'jam-disconnect sb-ext:*exit-hooks*)
  (sleep 0.1)
  (redis:connect))

(defun jam-disconnect ()
  "Disconnects from the current jam."
  (red:shutdown)
  (redis:disconnect)
  (setf sb-ext:*exit-hooks* (remove #'jam-disconnect sb-ext:*exit-hooks*)))
