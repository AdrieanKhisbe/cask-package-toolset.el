;;; test-helper --- Test helper for ${package-name}

;;; Commentary:
;; test helper inspired from https://github.com/tonini/overseer.el/blob/master/test/test-helper.el

;;; Code:

(require 'f)

(defvar cpt-path
  (f-parent (f-this-file)))

(defvar ${package-name}-test-path
  (f-dirname (f-this-file)))

(defvar ${package-name}-root-path
  (f-parent ${package-name}-test-path))

(defvar ${package-name}-sandbox-path
  (f-expand "sandbox" ${package-name}-test-path))

(when (f-exists? ${package-name}-sandbox-path)
  (error "Something is already in %s. Check and destroy it yourself" ${package-name}-sandbox-path))

(defmacro within-sandbox (&rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(let ((default-directory ${package-name}-sandbox-path))
     (when (f-exists? ${package-name}-sandbox-path)
       (f-delete default-directory :force))
     (f-mkdir ${package-name}-sandbox-path)
     ,@body
     (f-delete default-directory :force)))

(require 'ert)
(require 'el-mock)
(eval-when-compile
  (require 'cl))
(require '${package-name})

;;; test-helper.el ends here
