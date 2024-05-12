;;;; Data access functions.

(defvar *generators-directory* (format nil "~Agenerators/" *work-directory*))
(defvar *players-directory* (format nil "~Aplayers/" *work-directory*))

(ensure-directories-exist *generators-directory*)
(ensure-directories-exist *players-directory*)

(defun set-player (player-id player-data)
  "Sets the data of the player with the specified ID."
  (with-open-file
    (player-file (format nil "~A~A.lisp" *players-directory* player-id) :direction :output :if-exists :supersede)
    (prin1 (append player-data `(:last-seen ,(get-universal-time))) player-file)))

(defun get-player (player-id)
  "Returns data of player with the specified ID."
  (let* ((path (format nil "~A~A.lisp" *players-directory* player-id)))
    (if (uiop:file-exists-p path) (uiop:read-file-form path) nil)))

(defun pack-generator (generator-directory)
  "Packs the generator in the specified directory into a tgz file in the archive."
  (let* ((generator-name (file-namestring generator-directory))
         (generator-id (format nil "~A:~A" *player-id* generator-name)))
    (uiop:run-program (format nil "tar -czf ~A~A.tgz -C ~A ." *generators-directory* generator-id generator-directory))))

(defun get-generator-archive-path (generator-id)
  "Returns the path to the tgz file containing the generator with the specified ID."
  (let ((path (format nil "~A~A.tgz" *generators-directory* generator-id)))
    (if (uiop:file-exists-p path) path nil)))

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

(defun get-players ()
  "Returns the list of players."
  (directory-map
    (lambda (file-properties)
      (if (string= (getf file-properties :extension) "lisp") (get-player (getf file-properties :basename))))
    *players-directory*))

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

;;; Generator deployment.

(defun run-first-executable-in-path (path)
  "Iterate over the files in the specified path and run the first executable found."
  (dolist (file (uiop:directory-files path))
    (when (getf
            (org.shirakumo.file-attributes:decode-attributes (org.shirakumo.file-attributes:attributes path))
            :owner-execute)
      (uiop:launch-program file))))

(defun download-generator (generator-id)
  "Downloads the generator with the specified ID."
  ; TODO: Implement this.
  (declare (ignore generator-id)))

(defun play-generator (jam-name generator-name instance-id signature pubkey address)
  "Plays the generator with the specified ID."
  (unless (string= *current-jam* jam-name) (error "Not connected to jam ~A." jam-name))
  (let* ((player-id (pubkey-to-player-id pubkey))
         (player (get-player player-id)))
    (when player (unless (string= (getf player :pubkey) pubkey) (error "Pubkey mismatch.")))
    (validate-play-request jam-name generator-name instance-id signature pubkey address)
    (let* ((generator-id (format nil "~A:~A" player-id generator-name))
           (generator-archive-path (get-generator-archive-path generator-id))
           (generator-working-path (format nil "~A~A/~A/~A/" *jams-directory* player-id generator-name instance-id)))
      (when (eq generator-archive-path nil) (download-generator generator-id))
      (unless (uiop:directory-exists-p generator-working-path)
        (ensure-directories-exist generator-working-path)
        (uiop:run-program (format nil "tar -xzf ~A -C ~A" generator-archive-path generator-working-path)))
      (run-first-executable-in-path generator-working-path))
    (set-player player-id `(:pubkey ,pubkey :address ,address))))

(provide 'data-access)
