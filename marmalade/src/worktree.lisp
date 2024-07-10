;;;; Worktree functions.

(in-package :marmalade)

(defvar *generators-directory* (merge-pathnames #p"generators/" *work-directory*))
(defvar *jams-directory* (merge-pathnames #p"jams/" *work-directory*))
(defvar *players-directory* (merge-pathnames #p"players/" *work-directory*))
(unless (uiop:directory-exists-p *work-directory*) (error "Work directory ~A does not exist." *work-directory*))
(unless (uiop:file-exists-p *config-file-path*) (error "Config file ~A does not exist." *config-file-path*))

(ensure-directories-exist *generators-directory*)
(ensure-directories-exist *jams-directory*)
(ensure-directories-exist *players-directory*)

(defun directory-map (file-function path)
  "Returns a map of file-function over the files in path, in reverse chronological order, ignoring nils."
  (remove-if #'null
             (mapcar
               (lambda (file-path)
                 (let*
                   ((file-name (file-namestring file-path))
                    (file-name-parts (split-sequence:split-sequence #\. file-name))
                    (file-basename (first file-name-parts))
                    (file-extension (second file-name-parts)))
                   (funcall file-function `(:path ,file-path :basename ,file-basename :extension ,file-extension))))
               (sort (uiop:directory-files path) #'> :key #'file-write-date))))
