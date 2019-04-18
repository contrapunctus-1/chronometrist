# Project overview
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

All three components use timers to keep their buffers updated. [chronometrist-timer.el](chronometrist-timer.el) contains all timer-related code. Note - sometimes, when hacking, timers may cause subtle bugs which are very hard to debug. Restarting Emacs can fix them, so try that as a first sanity check.

# Browsing the code
I recommend using
* [nameless-mode](https://github.com/Malabarba/Nameless) for easier reading of Emacs Lisp code, and
* [visual-fill-column-mode](https://github.com/joostkremers/visual-fill-column) for easier reading of the Markdown files (without actually "filling" i.e. inserting newlines, so the same file is equally readable on any viewer/editor which supports line-wrapping).

# Point restore behaviour
After hacking, always test for and ensure the following -
1. Toggling the buffer via `chronometrist`/`chronometrist-report`/`chronometrist-statistics` should preserve point
   - TODO - actually, this should be implemented in a kill-buffer-hook, so it works whenever the buffer is killed, not just when we call the command.
2. The timer function should preserve point when the buffer is current
3. The timer function should preserve point when the buffer is not current, but is visible in another window
4. The next/previous week keys and buttons should preserve point.

# chronometrist-report date range logic
A quick description -
1. We get the current date in calendrical form using `(decode-time)`.
2. The variable `chronometrist-report-week-start-day` stores the day we consider the week to start with. The default is "Sunday".

   We check if we're on the week start day, else decrement the current date till we are, using `(chronometrist-report-previous-week-start)`.
3. We store that date in the global variable `chronometrist-report--ui-date`.
4. By counting up from `chronometrist-report--ui-date`, we get the dates for the week using `(chronometrist-report-date->dates-in-week)`. We store them in `chronometrist-report--ui-week-dates`.
5. We decrement/increment the global date by 7 and repeat the above process to get the previous/next week's dates (using `(chronometrist-report-previous-week)` and `(chronometrist-report-next-week)`).
