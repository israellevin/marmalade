;;;; HTTP client for player to player communication.

(in-package :marmalade)

(defun request (player-id endpoint &optional (method :get) (data nil))
  "Makes a request to the specified player on the specified endpoint with the specified data."
  (dex:request
    (format nil "~A/~A" (getf (get-player player-id) :address) endpoint)
    :method method :content data :keep-alive nil))

(defun request-json-as-plists (player-id endpoint &optional (method :get) (data nil))
  "Make a request to a player and convert the JSON response to a list of plists."
  (loop for hash-table across (com.inuoe.jzon:parse (request player-id endpoint method data))
        collect (loop for key being the hash-keys of hash-table using (hash-value value) append (list key value))))

(defun request-players (player-id)
  "Requests the list of players from the specified player."
  (request-json-as-plists player-id "players"))

(defun request-generators (player-id)
  "Requests the list of generators from the specified player."
  (request-json-as-plists player-id "generators"))

(defun request-generator (player-id generator-id)
  "Requests the specified generator from the specified player and download it."
  (request player-id (format nil "generator/~A" generator-id)))

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
           (generator (request-generator player-id generator-id)))
      (with-open-file
        (generator-file generator-path :direction :output :if-exists :supersede :element-type '(unsigned-byte 8))
        (write-sequence generator generator-file)))))

(defun request-play (player-id generator-name instance-id)
  "Requests the specified generator to be played by the specified player."
  (let ((signature (sign-play-request *jam-name* generator-name instance-id *player-address*)))
    (request player-id (format nil "play/~A/~A/~A" *jam-name* generator-name instance-id) :post
             `(("signature" . ,signature)
               ("pubkey" . ,*player-public-key-string*)
               ("address" . ,*player-address*)))))
