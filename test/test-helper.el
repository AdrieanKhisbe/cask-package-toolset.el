;; test helper inspired from https://github.com/tonini/overseer.el/blob/master/test/test-helper.el

;;; Code:

(require 'f)

(defvar cpt-path
  (f-parent (f-this-file)))

(defvar cpt-test-path
  (f-dirname (f-this-file)))

(defvar cpt-root-path
  (f-parent cpt-test-path))

(defvar cpt-sandbox-path
  (f-expand "sandbox" cpt-test-path))

(when (f-exists? cpt-sandbox-path)
  (error "Something is already in %s. Check and destroy it yourself" cpt-sandbox-path))

(defmacro within-sandbox (&rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(let ((default-directory cpt-sandbox-path))
     (when (f-exists? cpt-sandbox-path)
       (f-delete default-directory :force))
     (f-mkdir cpt-sandbox-path)
     ,@body
     (f-delete default-directory :force)))

(require 'ert)
(require 's)
(require 'dash)
(require 'undercover)
(undercover "*.el" "cask-package-toolset/*.el" (:exclude "*-test.el") (:send-report nil))
(require 'cask-package-toolset (f-expand "cask-package-toolset" cpt-root-path))

(provide 'test-helper)

;;; test-helper.el ends here
