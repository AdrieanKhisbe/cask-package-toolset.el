Feature: Set up Continous integration for my Library
  In order to have
  As an emacs developer
  I want to do set CI without hassle

  Scenario: Just run the command
    When I run package-toolset ""
    Then I should see command output:
       """
       Give us a command. install for instance, or consult usage with help
       """

  Scenario: Getting the status of cpt itself
    When I run package-toolset "status"
    Then I should see command output:
       """
       - Makefile            →   Ok
       - .gitignore          →   Ok
       - .travis.yml         →   Ok
       - Ert Test            →   Ok
       - Ecukes Features     →   Ok
       """

Scenario: Getting the status of a new project
    When I run package-toolset "status" within a new dir
    Then I should see command output:
       """
       - Makefile            →   You should run `package-toolset setup'
       - .gitignore          →   You should run `package-toolset setup'
       - .travis.yml         →   You should run `package-toolset setup'
       - Ert Test            →   You should run `cask exec package-toolset setup-ert'
       - Ecukes Features     →   You should run `cask exec ecukes new'
       """

  Scenario: Specy github repo
    When I run package-toolset "git --github-repo ab/cd"
    Then I should see command output:
       """
       ab/cd
       """
       # github repo was added to mock repo when git not available (for ci...)


  Scenario: Trying to print the badge with default syntax
    When I run package-toolset "badge -g AdrieanKhisbe/cask-package-toolset.el"
    Then I should see command output:
       """
       [![Build Status](https://travis-ci.org/AdrieanKhisbe/cask-package-toolset.el.svg)](https://travis-ci.org/AdrieanKhisbe/cask-package-toolset.el)
       [![Coverage Status](https://coveralls.io/repos/AdrieanKhisbe/cask-package-toolset.el/badge.svg)](https://coveralls.io/r/AdrieanKhisbe/cask-package-toolset.el)
       [![MELPA](http://melpa.org/packages/cask-package-toolset-badge.svg)](http://melpa.org/#/cask-package-toolset)
       [![MELPA Stable](http://stable.melpa.org/packages/cask-package-toolset-badge.svg)](http://stable.melpa.org/#/cask-package-toolset)
       [![Tag Version](https://img.shields.io/github/tag/AdrieanKhisbe/cask-package-toolset.el.svg)](https://github.com/AdrieanKhisbe/cask-package-toolset.el/tags)
       [![License](http://img.shields.io/:license-gpl3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)
       [![Join the chat](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/AdrieanKhisbe/cask-package-toolset.el)
      """

  Scenario: Trying to print the badge with orgmode syntax
    When I run package-toolset "badge -s orgmode -g AdrieanKhisbe/cask-package-toolset.el"
    Then I should see command output:
       """
       [[https://travis-ci.org/AdrieanKhisbe/cask-package-toolset.el][file:https://travis-ci.org/AdrieanKhisbe/cask-package-toolset.el.svg]]
       [[https://coveralls.io/r/AdrieanKhisbe/cask-package-toolset.el][file:https://coveralls.io/repos/AdrieanKhisbe/cask-package-toolset.el/badge.svg]]
       [[http://melpa.org/#/cask-package-toolset][file:http://melpa.org/packages/cask-package-toolset-badge.svg]]
       [[http://stable.melpa.org/#/cask-package-toolset][file:http://stable.melpa.org/packages/cask-package-toolset-badge.svg]]
       [[https://github.com/AdrieanKhisbe/cask-package-toolset.el/tags][file:https://img.shields.io/github/tag/AdrieanKhisbe/cask-package-toolset.el.svg]]
       [[http://www.gnu.org/licenses/gpl-3.0.html][file:http://img.shields.io/:license-gpl3-blue.svg]]
       [[https://gitter.im/AdrieanKhisbe/cask-package-toolset.el][file:https://badges.gitter.im/Join%20Chat.svg]]
       """


  Scenario: Trying to print the badge with rst syntax
    When I run package-toolset "badge -s rst -g AdrieanKhisbe/cask-package-toolset.el"
    Then I should see command output:
    """
    .. image:: https://travis-ci.org/AdrieanKhisbe/cask-package-toolset.el.svg
       :target: https://travis-ci.org/AdrieanKhisbe/cask-package-toolset.el
    .. image:: https://coveralls.io/repos/AdrieanKhisbe/cask-package-toolset.el/badge.svg
       :target: https://coveralls.io/r/AdrieanKhisbe/cask-package-toolset.el
    .. image:: http://melpa.org/packages/cask-package-toolset-badge.svg
       :target: http://melpa.org/#/cask-package-toolset
    .. image:: http://stable.melpa.org/packages/cask-package-toolset-badge.svg
       :target: http://stable.melpa.org/#/cask-package-toolset
    .. image:: https://img.shields.io/github/tag/AdrieanKhisbe/cask-package-toolset.el.svg
       :target: https://github.com/AdrieanKhisbe/cask-package-toolset.el/tags
    .. image:: http://img.shields.io/:license-gpl3-blue.svg
       :target: http://www.gnu.org/licenses/gpl-3.0.html
    .. image:: https://badges.gitter.im/Join%20Chat.svg
       :target: https://gitter.im/AdrieanKhisbe/cask-package-toolset.el
    """

  Scenario: Badge without github repo
    When I run package-toolset "badge -g "
    Then I should see command output:
       """
       We could not retrieve melpa recipe, specify the remote if origin does not refer to your github repository.
       """

  Scenario: Get melpa recipe
    When I run package-toolset "melpa-recipe -g cask/cask"
    Then I should see command output:
       """
       (cask :fetcher github :repo "cask/cask")
       """

  Scenario: Get melpa recipe with trailing .el
    When I run package-toolset "melpa-recipe -g cask/cask.el"
    Then I should see command output:
       """
       (cask :fetcher github :repo "cask/cask.el")
       """

  Scenario: Trying to setup ert when already done
    When I run package-toolset "setup-ert -g cask/cask.el"
    Then I should see command output:
       """
       Some test file already exist. If you want to erase them, add --force option
       """

  Scenario: Get undercover fragments
    When I run package-toolset "setup-undercover -g AdrieanKhisbe/cask-package-toolset.el"
    Then I should see command output:
       """
       ;; Here is the code you need to setup coverage:
       ;; for ert: insert this in "test/test-helper.el" before to require your package
       (require 'undercover)
       (undercover "*.el"
                   (:exclude "*-test.el")
                   (:send-report nil)
                   (:report-file "/tmp/undercover-report.json"))

       ;; for ecukes: insert this in "features/support/env.el" before to require your package
       (require 'undercover)
       (undercover "*.el"
                   (:exclude "*-test.el")
                   (:report-file "/tmp/undercover-report.json"))
       """
