;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

;; cask exec bin/package-toolset
;; §note: was tweak to enable coverage info to be collected!
(When "^I run package-toolset \"\\([^\"]*\\)\"$"
  (lambda (command)
    (let* ((buffer-name "*cpt-output*")
           (buffer
            (progn
              (when (get-buffer buffer-name)
                (kill-buffer buffer-name))
              (get-buffer-create buffer-name)))
           (default-directory (file-name-as-directory cask-package-toolset-project-path))
           (args
            (unless (equal command "")
              (s-split " " command)))
           (commander-ignore nil))
      (noflet ((message (msg &rest args)
                        (with-current-buffer buffer
                          (insert (apply #'format msg args))
                          (newline))))
               (commander-parse args))
      (with-current-buffer buffer
        (let ((content (ansi-color-filter-apply (buffer-string))))
          ;;(cond ((= exit-code 0) ; §later: see how to handle error?
                 (setq cask-package-toolset-stdout content))))))

(When "^I run package-toolset \"\\([^\"]+\\)\" within a new dir$"
      (lambda (command)
        (within-sandbox
         (let ((cask-package-toolset-project-path cask-package-toolset-sandbox-path))
           (When (format "I run package-toolset \"%s\"" command))))))

(Then "^I should see command output:$"
      (lambda (expected)
        (cask-package-toolset-should-match expected cask-package-toolset-stdout)))

(Then "^I should see command error:$"
      (lambda (expected)
        (cask-package-toolset-should-match expected cask-package-toolset-stderr)))

(defun cask-package-toolset-should-match (needle haystack)
  (should (s-contains? needle haystack)))
