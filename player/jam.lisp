;;;; Jam related functions.

(in-package :marmalade)

(defvar *jam-name* nil)
(defvar *jam-path* nil)
(defvar *jam-log-path* nil)

(defun jam-log (message &optional (log-file-path *jam-log-path*))
  "Logs a message to the jam log."
  (unless log-file-path (error "Not connected to a jam."))
  (with-open-file (log-file log-file-path :direction :output :if-does-not-exist :create :if-exists :append)
    (format log-file "~A ~A~%" (get-universal-time) message)))

(defun start-jam (jam-name)
  "Connects to a jam with the specified name."
  (let* ((jam-path (merge-pathnames (format nil "~A/" jam-name) *jams-directory*))
         (jam-log-path (merge-pathnames #P"jam.log" jam-path)))
    (ensure-directories-exist jam-path)
    (jam-log (format nil "connecting to jam ~A" jam-name) jam-log-path)
    (uiop:launch-program (format nil "marmalade-nslaunch --start-jam '~A'" jam-name))
    (setf *jam-name* jam-name)
    (setf *jam-path* jam-path)
    (setf *jam-log-path* jam-log-path))
  (push #'stop-jam sb-ext:*exit-hooks*))

(defun stop-jam ()
  "Disconnects from the current jam."
  (unless *jam-name* (error "Not connected to a jam."))
  (jam-log (format nil "disconnecting from jam ~A." *jam-name*))
  (uiop:launch-program (format nil "marmalade-nslaunch --stop-jam '~A'" *jam-name*))
  (setf sb-ext:*exit-hooks* (remove #'stop-jam sb-ext:*exit-hooks*)))
