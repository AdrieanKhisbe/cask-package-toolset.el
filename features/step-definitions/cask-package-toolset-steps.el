;; This file contains your project specific step definitions. All
;; files in this directory whose names end with "-steps.el" will be
;; loaded automatically by Ecukes.

(Given "^I have \"\\(.+\\)\"$"
  (lambda (something)
    ;; ...
    ))

(When "^I have \"\\(.+\\)\"$"
  (lambda (something)
    ;; ...
    ))

(Then "^I should have \"\\(.+\\)\"$"
  (lambda (something)
    ;; ...
    ))

(And "^I have \"\\(.+\\)\"$"
  (lambda (something)
    ;; ...
    ))

(But "^I should not have \"\\(.+\\)\"$"
  (lambda (something)
    ;; ...
    ))
;; §note: very very inspired of ecukes. (might be a good idea to generalize into an espurd)

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

(Then "^I should see command output:$"
      (lambda (expected)
        (cask-package-toolset-should-match expected cask-package-toolset-stdout)))

(Then "^I should see command error:$"
      (lambda (expected)
        (cask-package-toolset-should-match expected cask-package-toolset-stderr)))

(defun cask-package-toolset-should-match (needle haystack)
  (should (s-contains? needle haystack)))
