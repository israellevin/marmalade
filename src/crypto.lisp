;;;; Cryptographical operations.

(in-package :marmalade)

(defvar *crypto-debug* nil)

(defun get-player-id (key)
  "Returns the player ID from a given SSH key."
  (subseq (ssh-keys:fingerprint :sha256 key) 0 8))

(defun get-key-text (key)
  "Returns the SSH textual representation of a given SSH key."
  ;;Why is this so complicated, [dnaeon](https://github.com/dnaeon/cl-ssh-keys)? Am I missing something?
  (string-trim '(#\Newline)
               (with-output-to-string
                 (key-string)
                 (ssh-keys:write-key key key-string)
                 (format nil "~A" key-string))))

(defun parse-private-key-file (private-key-path)
  "Parses a private key from a given SSH private key file path - used for key of local player."
  (let* ((private-key (ssh-keys:parse-private-key-file private-key-path))
         (player-id (get-player-id private-key))
         (public-key-string (get-key-text (ssh-keys:embedded-public-key private-key))))
    (values player-id private-key public-key-string)))

(defun parse-public-key-string (public-key-string)
  "Parses a public key from a string in SSH public key format - used for keys of remote players."
  (let* ((public-key (ssh-keys:parse-public-key public-key-string))
         (player-id (get-player-id public-key))
         (public-key-string (get-key-text public-key)))
    (values player-id public-key public-key-string)))

(defun play-request-signable (jam-name generator-name instance-id address)
  "Generates a signable byte array for a play request."
  (ironclad:digest-sequence :sha256 (ironclad:ascii-string-to-byte-array
                                      (format nil "~A:~A:~A:~A" jam-name generator-name instance-id address))))

(defun sign-play-request (jam-name generator-name instance-id address)
  "Signs a play request."
  (base64:usb8-array-to-base64-string
    (ironclad:sign-message
      *player-key* (play-request-signable jam-name generator-name instance-id address))))

(defun check-signature (jam-name generator-name instance-id address public-key-string signature)
  "Checks the signature of a message."
  (multiple-value-bind
    (player-id public-key public-key-string)
    (parse-public-key-string public-key-string)
    (if *crypto-debug*
        (progn (warn "Signature checking is disabled for debugging - will accept any signature except \"invalid\".")
               (when (string= signature "invalid") (error "Invalid signature.")))
        (unless (ironclad:verify-signature
                  public-key
                  (play-request-signable jam-name generator-name instance-id address)
                  (base64:base64-string-to-usb8-array signature))
          (error "Invalid signature.")))
    (values player-id public-key public-key-string)))

(defun validate-play-request (jam-name generator-name instance-id address public-key-string signature)
  "Validates the play request and return the player ID."
  (multiple-value-bind
    (player-id public-key public-key-string)
    (check-signature jam-name generator-name instance-id address public-key-string signature)
    (upsert-player player-id public-key-string address)
    (values player-id public-key public-key-string)))

(defvar *player-address* (format nil "http://~A:~A" (get-config :host) (get-config :port)))
(multiple-value-bind (player-id key public-key-string) (parse-private-key-file (get-config :private-key-path))
  (defvar *player-id* player-id)
  (defvar *player-key* key)
  (defvar *player-public-key-string* public-key-string)
  (upsert-player *player-id* *player-public-key-string* *player-address*))
