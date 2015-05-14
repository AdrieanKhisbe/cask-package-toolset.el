;;; Test for `cask-package-toolset'
;;; Code:

;; §todo: make quiet test
(ert-deftest cpt-usage ()
  (should-not (s-blank? (cask-package-toolset-usage))))

(ert-deftest cpt-copy-template-non-existing-file ()
 ;; todo: see really how sandow works
  (let ((file-name "void") )
    (within-sandbox
     (message "def dir: %s" default-directory)
     (cask-package-toolset-copy-template file-name)
     (should (f-exists? (f-expand file-name))))))


(ert-deftest cpt-copy-template-should-not-erase-existing-file ()
  (let ((file-name "void") )
    (within-sandbox
     (cask-package-toolset-copy-template file-name)
     (should-error (cask-package-toolset-copy-template file-name)))))
