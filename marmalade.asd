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
  :components (
               (:file "package")
               (:file "common" :depends-on ("package"))
               (:file "crypto" :depends-on ("package"))
               (:file "jam" :depends-on ("common" "package"))
               (:file "player" :depends-on ("common" "crypto" "jam" "package"))
               (:file "generator" :depends-on ("common" "crypto" "package" "player"))
               (:file "p2p-client" :depends-on ("package"))
               (:file "p2p-server" :depends-on ("common" "generator" "package" "player" "p2p-client"))))
