;;;; HTTP client for player to player communication.

(in-package :marmalade)

(defun request-from-player (player-id endpoint &optional (method :get) (data nil))
  "Makes a request to the specified player on the specified endpoint with the specified data."
  (dex:request
    (format nil "~A/~A" (getf (get-player player-id) :address) endpoint)
    :method method :content data :keep-alive nil))

(defun request-form-from-player (player-id endpoint &optional (method :get) (data nil))
  "Make a request to a player and return the form data."
  (read-from-string (request-from-player player-id endpoint method data)))

(defun request-players-from-player (player-id)
  "Requests the list of players from the specified player."
  (request-form-from-player player-id "players"))

(defun request-generators-from-player (player-id)
  "Requests the list of generators from the specified player."
  (request-form-from-player player-id "generators"))

(defun request-generator-from-player (player-id generator-id)
  "Requests the specified generator from the specified player and download it."
  (request-from-player player-id (format nil "generator/~A" generator-id)))

(defun download-generator (generator-id &optional (player-id nil) (clobber nil))
  "Downloads the generator with the specified ID."
  (let ((generator-path (merge-pathnames (make-pathname :name generator-id :type "tgz") *generators-directory*)))
    (when (probe-file generator-path)
      (if clobber
          (warn "Clobbering existing generator.")
          (progn
            (warn "Generator already exists.")
            (return-from download-generator))))
    (let* ((player-id (or player-id (first (split-generator-id generator-id))))
           (generator (request-from-player player-id (format nil "generator/~A" generator-id))))
      (with-open-file
        (generator-file generator-path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
        (write-sequence generator generator-file)))))

(defun request-play (player-id generator-name instance-id)
  "Requests the specified generator to be played by the specified player."
  (let ((signature (sign-play-request *jam-name* generator-name instance-id *player-address*)))
    (request-from-player
      player-id (format nil "play/~A/~A/~A" *jam-name* generator-name instance-id) :post
      (prin1-to-string
        `((:signature . ,signature)
          (:pubkey . ,*player-public-key-string*)
          (:address . ,*player-address*))))))
