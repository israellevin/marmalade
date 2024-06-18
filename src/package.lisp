;;;; Marmalade package definition

(defpackage :marmalade
  (:use common-lisp)
  (:documentation "A system for musical collaboration")
  (:export
    #:*player-id*
    #:pack-generator
    #:jam-connect
    #:jam-disconnect
    #:request-play
    #:start-p2p-server
    #:stop-p2p-server))
