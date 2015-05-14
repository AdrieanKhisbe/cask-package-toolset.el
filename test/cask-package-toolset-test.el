;;; Test for `cask-package-toolset'

(ert-deftest cpt-usage ()
  (should-not (s-blank? (cask-package-toolset-usage))))

;;; Code:
