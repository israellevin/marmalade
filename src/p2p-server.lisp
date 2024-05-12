;;;; HTTP server for player to player communication.

(defun text-response (content http-request response-stream
                              &key (status 200) (response-string "OK") (mime "text/plain"))
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

(defun plists-response (plists http-request response-stream
                               &key (status 200) (response-string "OK") (mime "application/json"))
  "Generate and write an HTTP JSON response representing a list of plists."
  (text-response (com.inuoe.jzon:stringify
                   (mapcar
                     (lambda (plist)
                       (let ((json-object (make-hash-table :test 'equal)))
                         (loop for (key value) on plist by #'cddr
                               do (setf (gethash key json-object) value)) json-object)) plists))
                 http-request response-stream :status status :response-string response-string :mime mime))

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
      (:get-players (plists-response (get-players) http-request response-stream))
      (:get-generators (plists-response (get-generators) http-request response-stream))
      (:get-generator
        (let* ((generator-id (third path-parts))
               (file-path (get-generator-archive-path generator-id)))
          (if file-path (s-http-server::host-static-resource
                          http-request response-stream file-path :expires-max-age 0)
              (s-http-server:standard-http-html-error-response
                http-request response-stream 404
                "not found" (format nil "generator ~A was not found" generator-id)))))
      (:post-play (let* ((jam-name (third path-parts))
                         (generator-name (fourth path-parts))
                         (instance-id (fifth path-parts))
                         (headers (s-http-server:get-headers http-request))
                         (signature (getf headers :signature))
                         (pubkey (getf headers :pubkey))
                         (address (getf headers :address)))
                    (format t "playing generator ~A instance ~A signature ~A pubkey ~A~%" generator-name instance-id signature pubkey)
                    (if (play-generator jam-name generator-name instance-id signature pubkey address)
                        (text-response "OK" http-request response-stream)
                        (s-http-server:standard-http-html-error-response
                          http-request response-stream 400 "bad request" "failed to play generator"))
                    ))
      (:default (s-http-server:standard-http-html-error-response
                  http-request response-stream 404 "not found" (format nil "route ~A not supported" request-path)))
      (t (s-http-server:standard-http-html-error-response
           http-request response-stream 404 "not found" (format nil "route ~A not supported" request-path))))))

(defvar *network-server* (s-http-server:make-s-http-server :port (get-config :port)))
(s-http-server:register-context-handler *network-server* "/" 'network-request-handler)

(defun start-p2p-server () (s-http-server:start-server *network-server*))
(defun stop-p2p-server () (s-http-server:stop-server *network-server*))
