# Project overview
The files [chronometrist.el](chronometrist.el) and [chronometrist-report.el](chronometrist-report.el) contain definitions specific to the commands of the same name, most notably the major mode definitions, commands, and timers.

Both use (info "(elisp)Tabulated List Mode"). Each of them also contains a "-print-non-tabular" function, which prints the non-tabular parts of the buffer.

[chronometrist-lib.el](chronometrist-lib.el) contains definitions used by both chronometrist.el and chronometrist-report.el

I recommend using
* [nameless-mode](https://github.com/Malabarba/Nameless) for easier reading of Emacs Lisp code, and
* [visual-fill-column-mode](https://github.com/joostkremers/visual-fill-column) for easier reading of the Markdown files (without actually "filling" i.e. inserting newlines, so the same file is equally readable on any viewer/editor which supports line-wrapping).

Both [chronometrist.el](chronometrist.el) and [chronometrist-report.el](chronometrist-report.el) use timers to keep themselves updated. Sometimes, when hacking, the timers may cause subtle bugs which are very hard to debug. Restarting Emacs can fix them, so try that as a first sanity check.

# chronometrist-report date range logic
A quick description -
1. We get the current date in calendrical form using `(decode-time)`.
2. The variable `chronometrist-report-week-start-day` stores the day we consider the week to start with. The default is "Sunday".

   We check if we're on the week start day, else decrement the current date till we are, using `(chronometrist-report-previous-week-start)`.
3. We store that date in the global variable `chronometrist-report--ui-date`.
4. By counting up from `chronometrist-report--ui-date`, we get the dates for the week using `(chronometrist-report-date->dates-in-week)`. We store them in `chronometrist-report--ui-week-dates`.
5. We decrement/increment the global date by 7 and repeat the above process to get the previous/next week's dates (using `(chronometrist-report-previous-week)` and `(chronometrist-report-next-week)`).
