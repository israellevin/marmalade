;;;; Data access functions.

(ql:quickload '("cl-redis" "split-sequence" "uiop"))

(defvar *cwd* (uiop/os:getcwd))
(defvar *config-path* (format nil "~A/config.lisp" *cwd*))
(defvar *generators-path* (format nil "~A/generators" *cwd*))
(defvar *jams-path* (format nil "~A/jams" *cwd*))
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
  (let ((jam-path (format nil "~A/~A/" *jams-path* jam-name)))
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

(defun get-players (jam-name)
  "Returns the list of players."
  (unless *current-jam* (error "Not connected to a jam."))
  ;; TODO Should get this from scanning the jam directory, or maybe from redis.
  (list (list :name (get-config :name) :address (
                                                 format nil "http://~A:~A"
                                                 (get-config :host) (get-config :port)))))

(defun get-generators (jam-name)
  "Scans the generators directory and returns the list of generators."
  (mapcar (lambda (generator)
            (let*
              ((generator-file-name (file-namestring generator))
               (generator-id (first (split-sequence:split-sequence #\. generator-file-name)))
               (generator-parts (split-sequence:split-sequence #\: generator-id))
               (player-name (second generator-parts))
               (generator-name (third generator-parts)))
              (list
                :id generator-id :player player-name :name generator-name
                :url (format nil "http://~A:~A/generator/~A" (get-config :host) (get-config :port) generator-id))))
          (uiop:directory-files (format nil "~A/~A:*.tgz" *generators-path* jam-name))))

(defun get-generator (generator-id)
  "Returns the path to the tgz file containing the generator with the specified ID."
  (let* ((path (format nil "~A/~A.tgz" *generators-path* generator-id)))
    (cond ((uiop:file-exists-p path) path) (t nil))))

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
