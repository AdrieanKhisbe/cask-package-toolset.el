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
