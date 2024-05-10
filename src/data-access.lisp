;;;; Data access functions.

(require 'utils "src/utils.lisp")

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
               (generator-id (first (split #\. generator-file-name)))
               (generator-parts (split #\: generator-id))
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
