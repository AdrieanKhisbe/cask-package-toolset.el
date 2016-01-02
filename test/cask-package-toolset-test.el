;;; Test for `cask-package-toolset'
;;; Code:

(ert-deftest cpt-copy-template-non-existing-file ()
  (let ((file-name "void") )
    (within-sandbox
     (cask-package-toolset-copy-template file-name)
     (should (f-exists? (f-expand file-name))))))

(ert-deftest cpt-copy-non-existing-template()
  (should-error (cask-package-toolset-copy-template "non-existing-file")))

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

(ert-deftest cpt-install-template--ko()
  (within-sandbox
     (shut-up
      (f-touch (f-expand "void" ))
      (should-not (cask-package-toolset-install-template "void"))
      (should (s-contains? "Warning (emacs): File void is already existing. Skipping"
                     (shut-up-current-output))))))

(ert-deftest cpt-install-all()
  (within-sandbox
   (cask-package-toolset-install-all-templates)
   (should (-all? (lambda (file) (f-exists? file))
                  cask-package-toolset-templates))))

(ert-deftest cpt-setup-test-ok ()
  (shut-up
   (within-sandbox
    (let ((cask-package-toolset-github-repository "titi/toto"))
      (cask-package-toolset-setup-test)
      (should (s-contains? "Ert Scaffold files generated" (shut-up-current-output)))))))

(ert-deftest cpt-setup-test-ko-no-remote ()
  (shut-up
   (within-sandbox
    (cask-package-toolset-setup-test)
    (should (s-contains?
             "We could not retrieve project-name from github repo, specify the remote if origin does not refer to your github repository"
             (shut-up-current-output))))))


(ert-deftest cask-package-toolset-print-setup-coverage-ko ()
  (shut-up
   (within-sandbox
    (let ((cask-package-toolset-github-repository ""))
    (cask-package-toolset-print-setup-coverage)
    (should (s-contains?
             "We could not retrieve project-name from github repo, specify the remote if origin does not refer to your github repository"
             (shut-up-current-output)))))))

(ert-deftest cask-package-toolset-github-repository-name-infer-git ()
  (let ((cask-package-toolset-github-remote "github")
        (remote-url "git@github.com:AdrieanKhisbe/cask-package-toolset.el.git"))
    (with-mock
     (mock (shell-command-to-string "git config --get remote.github.url") => remote-url)
     (should (equal "AdrieanKhisbe/cask-package-toolset.el"
                    (cask-package-toolset-github-repository-name))))))

(ert-deftest cask-package-toolset-github-repository-name-infer-https ()
  (let ((cask-package-toolset-github-remote "github")
        (remote-url "https://github.com/AdrieanKhisbe/cask-package-toolset.el.git"))
    (with-mock
     (mock (shell-command-to-string "git config --get remote.github.url") => remote-url)
     (should (equal "AdrieanKhisbe/cask-package-toolset.el"
                    (cask-package-toolset-github-repository-name))))))

(ert-deftest cpt-update-travis--none ()
    (within-sandbox
     (shut-up (should-error (cask-package-toolset-ensure-latest-travis-config)))))

(ert-deftest cpt-update-travis--updated ()
  (within-sandbox
   (shut-up
    (f-write "let's say it's the travis file" 'utf-8 (f-expand ".travis.yml" cpt-sandbox-path))
    (cask-package-toolset-ensure-latest-travis-config)
    (should (s-contains? "Updating travis"
                         (shut-up-current-output))))))

(ert-deftest cpt-update-travis--up-to-date ()
  (within-sandbox
   (shut-up
    (cask-package-toolset-copy-template ".travis.yml")
    (cask-package-toolset-ensure-latest-travis-config)
    (should (s-contains? "Travis config already updated"
                         (shut-up-current-output))))))

(ert-deftest cpt-fill-template-ok()
  (should
   (equal
    (cask-package-toolset-fill-template "void-template" '(("package-name" . "Emacs")))
    "Hello Emacs\n")))

(ert-deftest cpt-fill-template-ko()
   (should-error (cask-package-toolset-fill-template "non-existing-file" '())))
   ;;§todo: check output "Warning (emacs): Template not-existing-file not found" (shut-up-current-output))

(ert-deftest cpt-install-test-template()
  (within-sandbox
   (cask-package-toolset-install-test-template "titi-toto")
   (should (f-exists? (f-expand "test/test-helper.el")))
   (should (f-exists? (f-expand "test/titi-toto-test.el")))))

;; Extractors

;; §TODO: real test of: repository-name with mocking

(ert-deftest cpt-github-url ()
  (let ((repo-name "AdrieanKhisbe/cask-package-toolset.el")
        (github-url "http://github.com/AdrieanKhisbe/cask-package-toolset.el"))
    (should (equal (cask-package-toolset-github-url repo-name) github-url))))

(ert-deftest cpt-travis-url ()
  (let ((repo-name "AdrieanKhisbe/cask-package-toolset.el")
        (travis-url "http://travis-ci.org/AdrieanKhisbe/cask-package-toolset.el"))
    (should (equal (cask-package-toolset-travis-url repo-name) travis-url))))

(ert-deftest cpt-project-name ()
  (let ((repo-name "abc/def")
        (project-name "def"))
    (should (equal (cask-package-toolset-project-name repo-name) project-name))))

(ert-deftest cpt-project-name-with-suffix ()
  (let ((repo-name "AdrieanKhisbe/cask-package-toolset.el")
        (project-name "cask-package-toolset"))
    (should (equal (cask-package-toolset-project-name repo-name) project-name))))

(ert-deftest cpt-project-name-with-prefix ()
  (let ((repo-name "toto/emacs-stuff")
        (project-name "stuff"))
    (should (equal (cask-package-toolset-project-name repo-name) project-name))))

;; Generators

(ert-deftest cpt-melpa-recipe-ok ()
  (let ((repo-name "AdrieanKhisbe/cask-package-toolset.el")
        (melpa-recipe
         "(cask-package-toolset :fetcher github :repo \"AdrieanKhisbe/cask-package-toolset.el\")"))
    (should (equal (cask-package-toolset-melpa-recipe repo-name) melpa-recipe))))

(ert-deftest cpt-melpa-recipe-ko ()
  (shut-up
   (with-mock
    (mock (cask-package-toolset-melpa-recipe *) => "")

    (cask-package-toolset-print-melpa-recipe)
    (should (s-contains?
             "We could not retrieve melpa recipe, specify the remote if origin does not refer to your github repository."
             (shut-up-current-output))))))

(ert-deftest cpt-travis-badge-get-template-ok ()
  (should (equal (cask-package-toolset-badge-template :travis :orgmode)
                 "[[https://travis-ci.org/${repository-name}][file:https://travis-ci.org/${repository-name}.svg]]")))

(ert-deftest cpt-travis-badge-get-template-ko ()
  (should-error  (cask-package-toolset-badge-template :real :bullshit)))

(ert-deftest cpt-travis-badge-org()
  (let ((repo-name "AdrieanKhisbe/cask-package-toolset.el")
        (travis-badge "[[https://travis-ci.org/AdrieanKhisbe/cask-package-toolset.el][file:https://travis-ci.org/AdrieanKhisbe/cask-package-toolset.el.svg]]"))
    (should (equal (cask-package-toolset-format-badge :travis repo-name :orgmode) travis-badge))))

(ert-deftest cpt-melpa-badge-markdown()
  (let ((repo-name "AdrieanKhisbe/cask-package-toolset.el")
        (melpa-badge "[![MELPA](http://melpa.org/packages/cask-package-toolset-badge.svg)](http://melpa.org/#/cask-package-toolset)"))
    (should (equal (cask-package-toolset-format-badge :melpa repo-name :markdown) melpa-badge))))

(ert-deftest cpt-melpa-stable-badge-html()
  (let ((repo-name "AdrieanKhisbe/cask-package-toolset.el")
        (melpa-stable-badge "<a href=\"http://stable.melpa.org/#/cask-package-toolset\"><img alt=\"MELPA Stable\" src=\"http://stable.melpa.org/packages/cask-package-toolset-badge.svg\"/></a>"))
    (should (equal (cask-package-toolset-format-badge :melpa-stable repo-name :html) melpa-stable-badge))))

;; §> options setters
(ert-deftest cask-package-toolset-set-badge-syntax-ko()
  (shut-up (should-error (cask-package-toolset-set-badge-syntax "junk"))))

(ert-deftest cask-package-toolset-set-badge-syntax-ok()
  (cask-package-toolset-set-badge-syntax "orgmode")
  (should (equal cask-package-toolset-badge-syntax :orgmode)))

(ert-deftest cask-package-toolset-set-force-ok()
  (should-not cask-package-toolset-force)
  (cask-package-toolset-set-force)
  (should cask-package-toolset-force))

(ert-deftest cask-package-toolset-set-remote-ok()
  (should (equal cask-package-toolset-github-remote "origin"))
  (cask-package-toolset-set-github-remote "whatevertheweather")
  (should (equal cask-package-toolset-github-remote "whatevertheweather")))

;; §todo: should check githubrepo workig when just mocking remote name (magit call)
