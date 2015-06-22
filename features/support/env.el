(require 'f)

(defvar cask-package-toolset-support-path
  (f-dirname load-file-name))

(defvar cask-package-toolset-features-path
  (f-parent cask-package-toolset-support-path))

(defvar cask-package-toolset-root-path
  (f-parent cask-package-toolset-features-path))

(add-to-list 'load-path cask-package-toolset-root-path)

(require 'undercover)
(undercover "*.el" "cask-package-toolset/*.el" (:exclude "*-test.el"))
(require 'cask-package-toolset)
(require 'espuds)
(require 'ert)

(Setup
 ;; Before anything has run
 )

(Before
 ;; Before each scenario is run
 )

(After
 ;; After each scenario is run
 )

(Teardown
 ;; After when everything has been run
 )
