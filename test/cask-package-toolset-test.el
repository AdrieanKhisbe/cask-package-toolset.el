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

;; Extractors

(ert-deftest cpt-github-url()
  (let ((repo-name "AdrieanKhisbe/cask-package-toolset.el")
        (github-url "http://github.com/AdrieanKhisbe/cask-package-toolset.el"))
    (should (equal (cask-package-toolset-github-url repo-name) github-url))))

(ert-deftest cpt-travis-url()
  (let ((repo-name "AdrieanKhisbe/cask-package-toolset.el")
        (travis-url "http://travis-ci.org/AdrieanKhisbe/cask-package-toolset.el"))
    (should (equal (cask-package-toolset-travis-url repo-name) travis-url))))

(ert-deftest cpt-project-name()
  (let ((repo-name "AdrieanKhisbe/cask-package-toolset.el")
        (project-name "cask-package-toolset"))
    (should (equal (cask-package-toolset-project-name repo-name) project-name))))

;; Generators

(ert-deftest cpt-melpa-recipe()
  (let ((repo-name "AdrieanKhisbe/cask-package-toolset.el")
        (melpa-recipe
         "(cask-package-toolset :fetcher github :repo \"AdrieanKhisbe/cask-package-toolset.el\")"))
    (should (equal (cask-package-toolset-melpa-recipe repo-name) melpa-recipe))))

(ert-deftest cpt-travis-badge-get-template-ok ()
  (should (equal (cask-package-toolset-badge-template :travis :orgmode)
                 "[[https://travis-ci.org/%s][file:https://travis-ci.org/%s.svg]]")))

(ert-deftest cpt-travis-badge-get-template-ko ()
  (should-error  (cask-package-toolset-badge-template :real :bullshit)))

(ert-deftest cpt-travis-badge-org()
  (let ((repo-name "AdrieanKhisbe/cask-package-toolset.el")
        (travis-badge "[[https://travis-ci.org/AdrieanKhisbe/cask-package-toolset.el][file:https://travis-ci.org/AdrieanKhisbe/cask-package-toolset.el.svg]]"))
    (should (equal (cask-package-toolset-travis-badge repo-name :orgmode) travis-badge))))
