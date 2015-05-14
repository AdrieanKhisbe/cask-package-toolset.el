;;; Test for `cask-package-toolset'
;;; Code:

;; §todo: make quiet test
(ert-deftest cpt-usage ()
  (should-not (s-blank? (cask-package-toolset-usage))))

(ert-deftest cpt-copy-template-non-existing-file ()
 ;; todo: see really how sandow works
  (let ((file-name "stub-file") )
    (within-sandbox cpt-sandbox
     (f-touch file-name) ; §todo: retrieve in template file?
     (cask-package-toolset-copy-template file-name)
     (should (f-exists? file-name))
     )))

(ert-deftest cpt-copy-template-should-not-erase-existing-file ())
