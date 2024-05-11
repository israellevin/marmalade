;;;; Data access functions.

(ql:quickload '("cl-redis" "split-sequence" "uiop"))

(defvar *cwd* (uiop/os:getcwd))
(defvar *config-path* (format nil "~Aconfig.lisp" *cwd*))
(defvar *generators-path* (format nil "~Agenerators/" *cwd*))
(defvar *jams-path* (format nil "~Ajams/" *cwd*))
(defvar *players-path* (format nil "~Aplayers/" *cwd*))
(defvar *current-jam* nil)

(defun get-config (key &optional (default nil))
  "Returns the value of the specified configuration key."
  (handler-case
    (let* ((config (uiop:read-file-form *config-path*))
           (pair (assoc key config)))
      (if pair
          (cdr pair)
          (if default default (error "Key ~A not found in configuration." key))))
    (error () default)))

(defun connect (jam-name)
  "Connects to a (potentially new) jam with the specified name."
  (let ((jam-path (format nil "~A~A/" *jams-path* jam-name)))
    (ensure-directories-exist jam-path)
    (uiop:launch-program (format nil "~A --dir ~A --save '' --appendonly yes --appenddirname redis"
                                 (get-config :redis-command) jam-path))
    (setf *current-jam* jam-name))
  (sleep 0.1)
  (redis:connect))

(defun disconnect ()
  "Disconnects from the current jam."
  (red:shutdown)
  (redis:disconnect))

(defun directory-map (file-function path)
  "Returns a map of file-function over the files in path, in reverse chronological order."
  (mapcar
    (lambda (file-path)
      (let*
        ((file-name (file-namestring file-path))
         (file-name-parts (split-sequence:split-sequence #\. file-name))
         (file-basename (first file-name-parts))
         (file-extension (second file-name-parts)))
        (funcall file-function `(:path ,file-path :basename ,file-basename :extension ,file-extension))))
    (sort (uiop:directory-files path) #'> :key #'file-write-date)))

(defun get-generators ()
  "Scans the generators directory and returns the list of generators."
  (directory-map
    (lambda (file-properties)
      (if (string= (getf file-properties :extension) "tgz")
          (let*
            ((generator-id (getf file-properties :basename))
             (generator-parts (split-sequence:split-sequence #\: generator-id))
             (player-name (first generator-parts))
             (generator-name (second generator-parts)))
            (list :id generator-id :player player-name :name generator-name)) nil)) *generators-path*))

(defun get-generator (generator-id)
  "Returns the path to the tgz file containing the generator with the specified ID."
  (let* ((path (format nil "~A~A.tgz" *generators-path* generator-id)))
    (cond ((uiop:file-exists-p path) path) (t nil))))

(defun get-players ()
  "Returns the list of players."
  (directory-map
    (lambda (file-properties)
      (if (string= (getf file-properties :extension) "lisp")
          (uiop:read-file-form (getf file-properties :path)))) *players-path*))

(defun play-generator (jam-name player-name generator-name instance-id signature pubkey address)
  "Plays the generator with the specified ID."
  (declare (ignore jam-name player-name generator-name instance-id signature pubkey address))
  ;; TODO:
  ;; If the player is known, verify that the pubkey matches the known pubkey.
  ;; Verify the signature and pubkey.
  ;; If the player is unknown, add the player to the list of players with the address and pubkey.
  ;; If the generator isn't known, download it from the player and deploy it.
  ;; Run the generator.
  )

(provide 'data-access)
