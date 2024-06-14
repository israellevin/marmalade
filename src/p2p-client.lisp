;;;; HTTP client for player to player communication.

(in-package :marmalade)

(defun get-request (peer-address endpoint)
  "Makes a GET request to the specified endpoint."
  (read-from-string (dex:get (format nil "~A/~A" peer-address endpoint) :keep-alive nil)))

(defun post-request (peer-address endpoint data)
  "Makes a POST request to the specified endpoint with the specified data."
  (dex:post (format nil "~A/~A" peer-address endpoint) :content data))

(defun download-generator (generator-id)
  "Downloads the generator with the specified ID."
  ; TODO: Implement this.
  (error "Downloading generators is not implemented yet."))
