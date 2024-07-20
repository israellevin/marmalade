;;; Generator related functions.

(in-package :marmalade)

(defun pack-generator (generator-directory)
  "Packs the generator in the specified directory into a tgz file in the archive."
  (let* ((generator-name (file-namestring generator-directory))
         (generator-id (format nil "~A:~A" *player-id* generator-name)))
    (uiop:run-program (format nil "tar -czf ~A~A.tgz -C ~A ." *generators-directory* generator-id generator-directory))))

(defun split-generator-id (generator-id)
  "Splits the generator ID into its player and name parts."
  (let ((delimiter-position (position #\: generator-id)))
    (list (subseq generator-id 0 delimiter-position) (subseq generator-id (1+ delimiter-position)))))

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
           (generator-parts (split-generator-id generator-id))
           (player-id (first generator-parts))
           (generator-name (second generator-parts)))
          (cons generator-id `(:name ,generator-name :player ,player-id))))) *generators-directory*))

(defun play-generator (jam-name generator-name instance-id address public-key-string signature-B64)
  "Plays the generator with the specified ID."
  (unless (string= *jam-name* jam-name) (error "Not connected to jam ~A." jam-name))
  (let* ((player-id
           (validate-play-request jam-name generator-name instance-id address public-key-string signature-B64))
         (player (get-player player-id)))
    (let* ((generator-id (format nil "~A:~A" player-id generator-name))
           (generator-archive-path (get-generator-archive-path generator-id))
           (generator-working-path
             (reduce (lambda (path part) (merge-pathnames (format nil "~A/" part) path))
                     (list *jam-name* player-id generator-name instance-id)
                     :initial-value *jams-directory*)))
      (when (eq generator-archive-path nil) (download-generator generator-id))
      (when (uiop:directory-exists-p generator-working-path)
          (error "Instance ~A of generator ~A has already been run." instance-id generator-name))
      (jam-log (format nil "Playing generator ~A." generator-name))
      (ensure-directories-exist generator-working-path)
      (uiop:run-program (format nil "marmalade-nslaunch --launch-generator '~A' '~A' '~A' '~A' &"
                                jam-name player-id generator-name instance-id)))))
