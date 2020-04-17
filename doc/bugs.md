# Bugs
## Chronometrist
1. timer function makes line highlight vanish
2. (goto-char (point-max)) -> RET -> the time spent on the last
   project in the list will be the first new project suggestion.
3. Create (and start) a _new_ project -> kill buffer -> run
   chronometrist -> cursor is not at the new project
   - can't reproduce it?
4. Start a project -> kill buffer -> run chronometrist -> cursor is
   at (point-max) instead of at project
5. `chronometrist-reason-list` sometimes returns garbage values. (?)
6. Digit argument doesn't work with buttons
## chronometrist-report
1. Start task close to midnight. If time spent on day 1 is X minutes and time spent on day 2 is Y minutes, time shown in chronometrist-report for day 1 is X+Y, and nothing is shown for day 2. (2020-03-10T00:45:47+0530)
