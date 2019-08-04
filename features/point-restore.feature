Feature: Point restore in Chronometrist

  Background:
  Given I open Chronometrist

  Scenario: Simple re-open
    When I close it
    And I open it again
    Then the position of point should be preserved

    Scenario: Timer with buffer current
      When buffer is current
      Then the timer should preserve point

      Scenario: Timer with buffer not current, but visible
        When buffer is not current
        But it is visible in another window
        Then the timer should preserve point

        Scenario: Previous/next week keys
          When I open chronometrist-report
          And I view the previous/next week
          Then the position point should be preserved
