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
       [![MELPA](http://melpa.org/packages/cask-package-toolset-badge.svg)](http://melpa.org/#/cask-package-toolset)
       [![MELPA stable](http://stable.melpa.org/packages/cask-package-toolset-badge.svg)](http://stable.melpa.org/#/cask-package-toolset)
       [![License] (http://img.shields.io/:license-gpl3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0.html)
       [![Join the chat](https://badges.gitter.im/Join%20Chat.svg)](https://gitter.im/AdrieanKhisbe/cask-package-toolset.el)
       """

  Scenario: Trying to print the badge with orgmode syntax
    When I run package-toolset "badge -s orgmode -g AdrieanKhisbe/cask-package-toolset.el"
    Then I should see command output:
       """
       [[https://travis-ci.org/AdrieanKhisbe/cask-package-toolset.el][file:https://travis-ci.org/AdrieanKhisbe/cask-package-toolset.el.svg]]
       [[http://melpa.org/#/cask-package-toolset][file:http://melpa.org/packages/cask-package-toolset-badge.svg]]
       [[http://stable.melpa.org/#/cask-package-toolset][file:http://stable.melpa.org/packages/cask-package-toolset-badge.svg]]
       [[http://www.gnu.org/licenses/gpl-3.0.html][http://img.shields.io/:license-gpl3-blue.svg]]
       [[https://gitter.im/AdrieanKhisbe/cask-package-toolset.el][file:https://badges.gitter.im/Join%20Chat.svg]]
       """

# TODO: with non existing remote.

  Scenario: Print help
    When I run package-toolset "help"
    Then I should see command output:
       """
       Help yourself, we'll help you.
       """
       # §FIXME do a real help 

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
    When I run package-toolset "setup-ert"
    Then I should see command output:
       """
       Some test file already exist. If you want to erase them, add --force option
       """
