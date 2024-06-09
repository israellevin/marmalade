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
               (:file "configuration" :depends-on ("package"))
               (:file "crypto" :depends-on ("package"))
               (:file "generator" :depends-on ("configuration" "crypto" "package" "player" "worktree"))
               (:file "jam" :depends-on ("configuration" "package"))
               (:file "p2p-client" :depends-on ("package"))
               (:file "p2p-server" :depends-on ("configuration" "generator" "package" "player" "p2p-client"))
               (:file "package")
               (:file "player" :depends-on ("configuration" "crypto" "jam" "package" "worktree"))
               (:file "worktree" :depends-on ("configuration" "package"))))
