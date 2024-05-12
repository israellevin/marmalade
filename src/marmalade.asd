;; Marmalade package.

(defsystem "marmalade"
  :description "marmalade: musical collaboration over a network in subjective real-time."
  :version "0.0.1"
  :author "I. <root@chakra>"
  :licence "Public Domain"
  :depends-on ("cl-redis" "com.inuoe.jzon" "file-attributes" "flexi-streams"
               "ironclad" "quri" "s-http-server" "split-sequence" "uiop"
               "verbose")
  :components ((:file "common")
               (:file "crypto")
               (:file "jam" :depends-on ("common"))
               (:file "data-access" :depends-on ("common" "crypto" "jam"))
               (:file "player-to-player" :depends-on ("common" "data-access"))
               (:file "run" :depends-on ("common" "player-to-player"))))
