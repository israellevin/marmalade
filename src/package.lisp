;;;; Marmalade package definition

(defpackage :marmalade
  (:use common-lisp)
  (:documentation "A system for musical collaboration")
  (:export
    #:*player-id*
    #:pack-generator
    #:play-generator
    #:request-play
    #:start-jam
    #:start-p2p-server
    #:stop-jam
    #:stop-p2p-server))
