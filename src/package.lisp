;;;; Marmalade package definition

(defpackage :marmalade
  (:use common-lisp)
  (:documentation "A system for musical collaboration")
  (:export
  #:pack-generator
  #:jam-connect
  #:jam-disconnect
  #:start-p2p-server
  #:stop-p2p-server))
