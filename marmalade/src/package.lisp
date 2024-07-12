;;;; Marmalade package definition

(defpackage :marmalade
  (:use common-lisp)
  (:documentation "A system for musical collaboration")
  (:export
    #:*player-id*
    #:download-generator
    #:pack-generator
    #:play-generator
    #:request-generators
    #:request-generator
    #:request-play
    #:request-players
    #:start-jam
    #:start-p2p-server
    #:stop-jam
    #:stop-p2p-server))
