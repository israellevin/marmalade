;;;; Marmalade player.

(ql:quickload '("com.inuoe.jzon" "quri" "s-http-server" "split-sequence" "uiop"))

;;; Data access functions.

(defvar *cwd* (uiop/os:getcwd))
(defvar *config-path* (format nil "~A/config.lisp" *cwd*))
(defvar *generators-path* (format nil "~A/generators" *cwd*))

(defun get-config (key &optional (default nil))
  "Returns the value of the specified configuration key."
  (handler-case
    (let* ((config (uiop:read-file-form *config-path*))
           (pair (assoc key config)))
      (if pair
          (cdr pair)
          (if default default (error "Key ~A not found in configuration." key))))
    (error () default)))

(defvar *players* (list (list :name (get-config :name) :address (
                                                                 format nil "http://~A:~A"
                                                                 (get-config :host) (get-config :port)))))

(defun get-players (jam-name)
  "Returns the list of players."
  (declare (ignore jam-name))
  ;; TODO Should get this from redis.
  *players*)

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

;;; HTTP server functions.

(defun text-response (http-request response-stream content
                                   &optional (status 200) (response-string "OK") (mime "text/plain"))
  "Generate and write an HTTP text response."
  (when response-stream
    (s-http-server:write-http-response-status-line response-stream status response-string
                                                   (s-http-server:get-http-version http-request))
    (s-http-server:write-http-response-headers
      (s-http-server:standard-http-response-headers
        http-request
        :content-type mime
        :content-length (length content))
      response-stream)
    (s-http-server:write-http-response-line "" response-stream)
    (write-string content response-stream)
    (length content)))

(defun plists-response (http-request response-stream plists
                                     &optional (status 200) (response-string "OK") (mime "application/json"))
  "Generate and write an HTTP JSON response representing a list of plists."
  (let* ((json-objects (mapcar
                         (lambda (plist)
                           (let ((json-object (make-hash-table :test 'equal)))
                             (loop for (key value) on plist by #'cddr
                                   do (setf (gethash key json-object) value)) json-object)) plists)))
    (text-response http-request response-stream (com.inuoe.jzon:stringify json-objects) status response-string mime)))

(defun network-request-handler (server handler http-request response-stream)
  "Handles requests."
  (let* ((request-path (quri:url-decode (s-http-server:get-path http-request)))
         (path-parts (split-sequence:split-sequence #\/ request-path))
         (endpoint-name (second path-parts))
         (endpoint-id (intern
                        (string-upcase (format nil "~A-~A" (s-http-server:get-method http-request) endpoint-name))
                        :keyword)))
    (case endpoint-id
      (:get-stats (s-http-server:s-http-server-handler server handler http-request response-stream))
      (:get-players (plists-response http-request response-stream (get-players (get-config :current-jam))))
      (:get-generators (plists-response http-request response-stream (get-generators (get-config :current-jam))))
      (:get-generator
        (let* ((generator-id (third path-parts))
               (file-path (get-generator generator-id)))
          (if file-path (s-http-server::host-static-resource
                          http-request response-stream file-path :expires-max-age 0)
              (s-http-server:standard-http-html-error-response
                http-request response-stream 404
                "not found" (format nil "generator ~A was not found" generator-id)))))
      (:post-play (text-response http-request response-stream "not yet" 501 "Not Implemented"))
      (t (s-http-server:standard-http-html-error-response
           http-request response-stream 404 "not found" (format nil "route ~A not supported" request-path))))))

(defvar *network-server* (s-http-server:make-s-http-server :port (get-config :port)))
(s-http-server:register-context-handler *network-server* "/" 'network-request-handler)
(s-http-server:start-server *network-server*)
