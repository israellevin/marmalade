;;;; Marmalade player.
(ql:quickload '("s-http-server" "split-sequence" "uiop" "com.inuoe.jzon"))

(defun get-config (key &optional (default nil))
  "Returns the value of the specified configuration key."
  (handler-case
    (let* ((config (uiop:read-file-form "/root/src/marmalade/config.lisp"))
           (pair (assoc key config)))
      (if pair
          (cdr pair)
          (if default default (error "Key ~A not found in configuration." key))))
    (error () default)))

(defvar *players*
    (list
      (let ((player (make-hash-table :test 'equal)))
        (setf (gethash "name" player) (get-config :name))
        (setf (gethash "address" player) (format nil "http://~a:~a" (get-config :host) (get-config :port))) player)
      (let ((player (make-hash-table :test 'equal)))
        (setf (gethash "name" player) "foo1")
        (setf (gethash "address" player) "bar1") player)))
(defun get-players (jam-name)
  "Returns the list of players."
  ;; TODO Should scan `generators` and individual players directories.
  *players*)

(defvar *generators* nil)
(defun get-generators (jam-name)
  "Returns the list of generators."
  ;; TODO Should scan `generators` directory.
  *generators*)

(defun play-generator (jam-name player-name generator-name instance-id signature pubkey address)
  "Plays the generator with the specified ID."
  ;; TODO:
  ;; If the player is known, verify that the pubkey matches the known pubkey.
  ;; Verify the signature and pubkey.
  ;; If the player is unknown, add the player to the list of players with the address and pubkey.
  ;; If the generator isn't known, download it from the player and deploy it.
  ;; Run the generator.
  )

(defun text-response (http-request stream content &optional (status 200) (string "OK") (mime "text/plain"))
  "Generate and write a standard HTTP response."
  (when stream
    (s-http-server:write-http-response-status-line stream status string (s-http-server:get-http-version http-request))
    (s-http-server:write-http-response-headers
      (s-http-server:standard-http-response-headers
        http-request
        :content-type mime
        :content-length (length content))
      stream)
    (s-http-server:write-http-response-line "" stream)
    (write-string content stream)
    (length content)))

(defun json-response (http-request stream content &optional (status 200) (string "OK") (mime "application/json"))
  "Generate and write a standard HTTP response."
  (text-response http-request stream (com.inuoe.jzon:stringify content) status string mime))

(defun network-request-handler (s-http-server handler http-request stream)
  "Handles requests from other players."
  (let* ((request-method (s-http-server:get-method http-request))
         (request-path (s-http-server:get-path http-request))
         (path-parts (split-sequence:split-sequence #\/ request-path))
         (endpoint (second path-parts)))
    (cond
      ((string= request-method "GET")
       (cond
         ((string= endpoint "stats")
          (s-http-server:s-http-server-handler s-http-server handler http-request stream))
         ((string= endpoint "players")
          (json-response http-request stream (get-players (get-config :jam-name))))
         ((string= endpoint "generators")
          (json-response http-request stream (get-generators (get-config :jam-name))))
         ((string= endpoint "generator")
          (let ((generator-id (third path-parts)))
            (s-http-server:standard-http-html-message-response
              http-request stream "Not Downloading Generator" (format nil "~A" generator-id))))
         (t
          (s-http-server:standard-http-html-error-response
            http-request stream 404 "not found" "The requested resource was not found."))))
      ((string= request-method "POST")
       (cond
         ((string= endpoint "play")
          (text-response http-request stream "not yet" 501 "Not Implemented"))
         (t
          (s-http-server:standard-http-html-error-response
            http-request stream 404 "not found" "The requested resource was not found.")))))))

(defvar *network-server* (s-http-server:make-s-http-server :port (get-config :port)))
(s-http-server:register-context-handler *network-server* "/" 'network-request-handler)
(s-http-server:start-server *network-server*)
