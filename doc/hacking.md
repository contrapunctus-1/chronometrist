# Hacking
## Project overview
Chronometrist has three components, and each has a file containing major mode definitions and user-facing commands.
- [chronometrist.el](chronometrist.el)
- [chronometrist-report.el](chronometrist-report.el)
- [chronometrist-statistics.el](chronometrist-statistics.el)

All three of these use (info "(elisp)Tabulated List Mode"). Each of them also contains a "-print-non-tabular" function, which prints the non-tabular parts of the buffer.

Each of them has a corresponding `-custom` file, which contain the Customize group and custom variable definitions for user-facing variables -
- [chronometrist-custom.el](chronometrist-custom.el)
- [chronometrist-report-custom.el](chronometrist-report-custom.el)
- [chronometrist-statistics-custom.el](chronometrist-statistics-custom.el)

[chronometrist-common.el](chronometrist-common.el) contains definitions common to all components.

All three components use timers to keep their buffers updated. [chronometrist-timer.el](chronometrist-timer.el) contains all timer-related code. Note - sometimes, when hacking or dealing with errors, timers may result in subtle bugs which are very hard to debug. Using `chronometrist-force-restart-timer` or restarting Emacs can fix them, so try that as a first sanity check.

## Browsing the code
I recommend using
* [nameless-mode](https://github.com/Malabarba/Nameless) for easier reading of Emacs Lisp code, and
* [visual-fill-column-mode](https://github.com/joostkremers/visual-fill-column) for easier reading of the Markdown files (without actually "filling" i.e. inserting newlines, so the same file is equally readable on any viewer/editor which supports line-wrapping).

## Point restore behaviour
After hacking, always test for and ensure the following -
1. Toggling the buffer via `chronometrist`/`chronometrist-report`/`chronometrist-statistics` should preserve point
   - TODO - actually, this should be implemented in a kill-buffer-hook, so it works whenever the buffer is killed, not just when we call the command.
2. The timer function should preserve point when the buffer is current
3. The timer function should preserve point when the buffer is not current, but is visible in another window
4. The next/previous week keys and buttons should preserve point.

## chronometrist-report date range logic
A quick description, starting from the first time `chronometrist-report` is run in an Emacs session -
1. We get the current date as a ts struct `(chronometrist-date)`.
2. The variable `chronometrist-report-week-start-day` stores the day we consider the week to start with. The default is "Sunday".

   We check if the date from #2 is on the week start day, else decrement it till we are, using `(chronometrist-report-previous-week-start)`.
3. We store the date from #3 in the global variable `chronometrist-report--ui-date`.
4. By counting up from `chronometrist-report--ui-date`, we get dates for the days in the next 7 days using `(chronometrist-report-date->dates-in-week)`. We store them in `chronometrist-report--ui-week-dates`.

   The dates in `chronometrist-report--ui-week-dates` are what is finally used to query the data displayed in the buffer.
5. To get data for the previous/next weeks, we decrement/increment the date in `chronometrist-report--ui-date` by 7 days and repeat the above process (via `(chronometrist-report-previous-week)`/`(chronometrist-report-next-week)`).
