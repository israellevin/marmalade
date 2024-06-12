;;; Generator related functions.

(in-package :marmalade)

(defun pack-generator (generator-directory)
  "Packs the generator in the specified directory into a tgz file in the archive."
  (let* ((generator-name (file-namestring generator-directory))
         (generator-id (format nil "~A:~A" *player-id* generator-name)))
    (uiop:run-program (format nil "tar -czf ~A~A.tgz -C ~A ." *generators-directory* generator-id generator-directory))))

(defun get-generator-archive-path (generator-id)
  "Returns the path to the tgz file containing the generator with the specified ID."
  (let ((path (merge-pathnames (make-pathname :name generator-id :type "tgz") *generators-directory*)))
    (if (uiop:file-exists-p path) path nil)))

(defun get-generators ()
  "Scans the generators directory and returns the list of generators."
  (directory-map
    (lambda (file-properties)
      (when (string= (getf file-properties :extension) "tgz")
        (let*
          ((generator-id (getf file-properties :basename))
           (generator-parts (split-sequence:split-sequence #\: generator-id))
           (player-id (first generator-parts))
           (generator-name (second generator-parts)))
          `(:id ,generator-id :name ,generator-name :player ,player-id)))) *generators-directory*))

(defun run-first-executable-in-path (path)
  "Iterate over the files in the specified path and run the first executable found."
  (dolist (file (uiop:directory-files path))
    (when (getf
            (org.shirakumo.file-attributes:decode-attributes (org.shirakumo.file-attributes:attributes file))
            :owner-execute)
      (uiop:launch-program (namestring file)))))

(defun play-generator (jam-name generator-name instance-id address signature-B64 public-key-string)
  "Plays the generator with the specified ID."
  (unless (string= *current-jam* jam-name) (error "Not connected to jam ~A." jam-name))
  (let* ((player-id
           (validate-play-request jam-name generator-name instance-id address signature-B64 public-key-string))
         (player (get-player player-id)))
    (let* ((generator-id (format nil "~A:~A" player-id generator-name))
           (generator-archive-path (get-generator-archive-path generator-id))
           (generator-working-path
             (reduce (lambda (path part) (merge-pathnames (format nil "~A/" part) path))
                     (list *current-jam* player-id generator-name instance-id)
                     :initial-value *jams-directory*)))
      (when (eq generator-archive-path nil) (download-generator generator-id))
      (unless (uiop:directory-exists-p generator-working-path)
        (ensure-directories-exist generator-working-path)
        (uiop:run-program (format nil "tar -xzf ~A -C ~A" generator-archive-path generator-working-path)))
      (run-first-executable-in-path generator-working-path))))
