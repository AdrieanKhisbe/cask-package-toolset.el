(require 'f)
(require 'noflet)

(defvar cask-package-toolset-support-path
  (f-dirname load-file-name))

(defvar cask-package-toolset-features-path
  (f-parent cask-package-toolset-support-path))

(defvar cask-package-toolset-root-path
  (f-parent cask-package-toolset-features-path))

(defvar cask-package-toolset-bin-path
  (f-expand "bin" cask-package-toolset-root-path))

(defvar cask-package-toolset-executable
  (f-expand "package-toolset" cask-package-toolset-bin-path))

(defvar cask-package-toolset-sandbox-path
  (f-expand "sandbox" cask-package-toolset-root-path))

;; §todo: set it with a given!
(defvar cask-package-toolset-project-path
  cask-package-toolset-root-path)

(defvar cask-package-toolset-stderr)
(defvar cask-package-toolset-stdout)

(defmacro within-sandbox (&rest body)
  "Evaluate BODY in an empty sandbox directory."
  `(let ((default-directory cask-package-toolset-sandbox-path))
     (when (f-exists? cask-package-toolset-sandbox-path)
       (f-delete default-directory :force))
     (f-mkdir cask-package-toolset-sandbox-path)
     ,@body
     (f-delete default-directory :force)))

(add-to-list 'load-path cask-package-toolset-root-path)

(require 'undercover)
(undercover "*.el"
            (:exclude "*-test.el")
            (:report-file "/tmp/undercover-report.json"))

(defvar commander-ignore t
  "With this prevent commander to run without asking")
(require 'cask-package-toolset)
(require 'espuds)
(require 'ert)
(require 'ansi-color)

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
