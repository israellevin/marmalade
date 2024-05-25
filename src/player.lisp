;;;; Player related functions.

(in-package :marmalade)

(defvar *players-directory* (format nil "~Aplayers/" *work-directory*))
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

(defun get-players ()
  "Returns the list of players."
  (directory-map
    (lambda (file-properties)
      (if (string= (getf file-properties :extension) "lisp") (get-player (getf file-properties :basename))))
    *players-directory*))

(defvar *address* (format nil "http://~A:~A" (get-config :host) (get-config :port)))
(defvar *pubkey* (uiop:read-file-string (get-config :pubkey-path)))
(defvar *player-id* (pubkey-to-player-id *pubkey*))
(set-player *player-id* `(:pubkey ,*pubkey* :address ,*address*))
