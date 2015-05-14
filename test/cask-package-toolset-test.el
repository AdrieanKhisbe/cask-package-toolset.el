;;; Test for `cask-package-toolset'
;;; Code:

;; §todo: make quiet test

(ert-deftest cpt-usage ()
  (should-not (s-blank? (cask-package-toolset-usage))))

(ert-deftest cpt-copy-template-non-existing-file ()
  (let ((file-name "void") )
    (within-sandbox
     (cask-package-toolset-copy-template file-name)
     (should (f-exists? (f-expand file-name))))))

(ert-deftest cpt-copy-template-should-not-erase-existing-file ()
  (let ((file-name "void") )
    (within-sandbox
     (cask-package-toolset-copy-template file-name)
     (should-error (cask-package-toolset-copy-template file-name)))))

(ert-deftest cpt-template-present--if-present ()
  (let ((file-name "void") )
    (within-sandbox
     (cask-package-toolset-copy-template file-name)
     (should  (cask-package-toolset-template-present-p file-name)))))

(ert-deftest cpt-template-present--if-absent ()
    (let ((file-name "void") )
      (within-sandbox
       (should-not (cask-package-toolset-template-present-p file-name)))))

(ert-deftest cpt-install-all()
  (within-sandbox
   (cask-package-toolset-install-all-templates)
   (should (-all? (lambda (file) (f-exists? file))
                  cask-package-toolset-templates))))
