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
