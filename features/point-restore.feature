Feature: Point restore in Chronometrist

  # What about when we want point to be on the last project instead? In which situations should that happen?

  Background:
  Given I open Chronometrist
  And I go to a random point in the buffer

  Scenario: Simple re-open 1
    When I kill its buffer
    And I open it again
    Then the position of point should be preserved

    Scenario: Simple re-open 2
      When I toggle its buffer
      And I open it again
      Then the position of point should be preserved

      Scenario: Timer with buffer current
        When buffer is current
        Then the timer should preserve the position of point

        Scenario: Timer with buffer not current, but visible
          When buffer is not current
          But it is visible in another window
          Then the timer should preserve the position of point

          Scenario: Previous/next week keys
            When I open chronometrist-report
            And I view the previous/next week
            Then the position of point should be preserved
