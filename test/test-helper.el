;; test helper inspired from https://github.com/tonini/overseer.el/blob/master/test/test-helper.el

;;; Code:

(require 'f)

(defvar cask-package-toolset-test-path
  (f-parent (f-this-file)))

(defvar cask-package-toolset-root-path
  (f-parent cask-package-toolset-test-path))

(defvar cask-package-toolset-test-path
  (f-dirname (f-this-file)))

(defvar cask-package-toolset-sandbox-path
  (f-expand "sandbox" cask-package-toolset-test-path))

(defmacro within-sandbox (&optional current &rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(let ((default-directory
           (f-join cask-package-toolset-sandbox-path (format "%s" ,current))))
     (f-mkdir cask-package-toolset-sandbox-path)
     ,@body
     (f-delete cask-package-toolset-sandbox-path :force)))

(require 'ert)
(require 'cask-package-toolset (f-expand "cask-package-toolset" cask-package-toolset-root-path))

(provide 'test-helper)

;;; test-helper.el ends here
