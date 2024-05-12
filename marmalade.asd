;; Marmalade package.

(defsystem "marmalade"
  :description "marmalade: musical collaboration over a network in subjective real-time."
  :version "0.0.1"
  :author "I. <root@chakra>"
  :licence "Public Domain"
  :depends-on ("cl-redis" "com.inuoe.jzon" "file-attributes" "flexi-streams"
               "ironclad" "quri" "s-http-server" "split-sequence" "uiop"
               "verbose")
  :pathname "src"
  :components ((:file "common")
               (:file "crypto")
               (:file "jam" :depends-on ("common"))
               (:file "player" :depends-on ("common" "crypto" "jam"))
               (:file "generator" :depends-on ("common" "crypto" "player"))
               (:file "p2p-client")
               (:file "p2p-server" :depends-on ("common" "generator" "player" "p2p-client"))))
