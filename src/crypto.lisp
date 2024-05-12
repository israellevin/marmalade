;;;; Cryptographical operations.

(defun pubkey-to-player-id (pubkey)
  "Returns the player ID - first 8 characters of SHA256 of the pubkey."
  (subseq
    (ironclad:byte-array-to-hex-string (ironclad:digest-sequence :sha256 (flexi-streams:string-to-octets pubkey)))
    0 8))

(defun validate-play-request (jam-name generator-name instance-id signature pubkey address)
  "Validates the play request."
  ; TODO: Implement this.
  (declare (ignore jam-name generator-name instance-id signature pubkey address))
  t)

(provide 'crypto)
