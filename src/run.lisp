;;;; Marmalade runner.

(defvar *address* (format nil "http://~A:~A" (get-config :host) (get-config :port)))
(defvar *pubkey* (uiop:read-file-string (get-config :pubkey-path)))
(defvar *player-id* (pubkey-to-player-id *pubkey*))

(set-player *player-id* `(:pubkey ,*pubkey* :address ,*address*))

(start-p2p-server)
