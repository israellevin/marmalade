;;;; Player related functions.

(in-package :marmalade)

(defun player-pathname (player-id)
  "Returns the pathname of the file containing the player data."
  (merge-pathnames (make-pathname :name player-id :type "lisp") *players-directory*))

(defun get-player (player-id)
  "Returns data of player with the specified ID."
  (let ((path (player-pathname player-id))) (if (uiop:file-exists-p path) (uiop:read-file-form path) nil)))

(defun get-players ()
  "Returns the list of players."
  (directory-map
    (lambda (file-properties)
      (if (string= (getf file-properties :extension) "lisp") (get-player (getf file-properties :basename))))
    *players-directory*))

(defun upsert-player (player-id public-key address)
  "Upserts a player with the specified ID, public key and address."
  (let ((player (get-player player-id)))
    (and player (not (string= (getf player :public-key) public-key)) (error "Public key mismatch.")))
  (with-open-file
    (player-file (player-pathname player-id) :direction :output :if-exists :supersede)
    (prin1 `(:public-key ,public-key :address ,address :last-seen ,(get-universal-time)) player-file))
  (get-player player-id))
