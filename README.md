# timeclock-ui
A nice user interface for Emacs' timeclock.el

Largely modelled after the Android application, [A Time Tracker](https://github.com/netmackan/ATimeTracker)

## Installation
`timeclock-ui` requires [dash.el](https://github.com/magnars/dash.el)

Add timeclock-ui to your load-path, and `(require 'timeclock-ui)`.

## Commands
### timeclock-list
Run `M-x timeclock-list` to see your projects, the time you spent on them today, which one is active, and the total time clocked today.

Hit `RET` on a project to clock in for it. If it's already clocked in, it will be clocked out (and you'll be prompted for an optional reason).

Whenever you call `timeclock-list`, the cursor will helpfully be placed on the last activity you clocked out of or the current activity clocked in.

Alternatively, hit `<numeric prefix> RET` anywhere in the buffer to toggle the corresponding project, e.g. `C-1 RET` will toggle the project with index 1.

Press `l` to view your `timeclock-file` (`~/.emacs.d/timelog` by default). Press `r` to see a weekly report (see `timeclock-report`)

Running `M-x timeclock-list` when the timeclock-list is visible will kill it, so the key you bind it to can function as a toggle.

`timeclock-list` keeps itself updated via an idle timer - no pressing `g` to update.

### timeclock-report
Run `M-x timeclock-report` (or `timeclock-list` with a prefix argument, or press `r` in the `timeclock-list` buffer) to see a weekly report.

Press `b` to look at past weeks, and `f` for future weeks.

Press `l` to view your `timeclock-file`, `~/.emacs.d/timelog` by default.

Just like `timeclock-list`, `timeclock-report` will also toggle the visibilty of the buffer.

`timeclock-report` keeps itself updated via an idle timer - no pressing `g` to update.

## Customization
### timeclock-list-hide-cursor
Set this to non-nil if you want to hide the cursor in timeclock-list. The line the cursor is on will be highlighted instead.

## Roadmap
* New commands for statistics
  - activity-specific - average time spent in $time_period, average days worked on in $time_period, current/longest streaks, ...
  - general - most productive $time_period, GitHub-style work heatmap calendar, ...
* Update when the timeclock-file changes using a filesystem watcher
* Mouse support
* [inflatable raptor](https://github.com/MichaelMure/git-bug/#planned-features)

### timeclock-list
1. Add variable to let user control prompting-for-reason behaviour
2. **Custom day start/end time** - option to use a specific time to define when a day starts/ends. e.g. 08:00 will mean a day starts and ends at 08:00 instead of the usual 24:00/00:00. Helpful for late sleepers.
3. **Better shortcuts** - Shortcuts derived from the first alphabet of each project could be even nicer (but the code to generate them from similarly-named projects would be somewhat complex...)
4. Make clocked-in project row bold, either in addition to the star, or replacing it.
5. **Modeline support** - show currently active project + time spent on it so far in the mode-line (see timeclock-mode-line-display)
6. **Fix completion** - the default reason suggested is the last one used. Can't even begin to explain how nonsensical that is. (might be an ido or timeclock.el problem)
   - Make default blank?
   - Possibly make suggestions activity-sensitive e.g. when stopping activity A, don't suggest past reasons used for activity B, C, etc. Add a variable to customize this, because it might not be the behaviour everyone wants.
7. **Improve help**
   - Show shortcuts by consulting the keymap rather than using a hardcoded string.
   - Change 'see weekly report' and 'open log file' to buttons
8. Refresh when you select the list buffer (impossible? make-thread in v26? Use emacs-async library?)

### timeclock-report
1. Highlight column of current day
2. Add support for other locale weeks/weekday names

### Miscellaneous
1. README - add images, link to a Jabber webclient
2. [-] Document API somewhere (list of functions, their argument lists, and outputs)
3. Use Cask?
4. Create test timelog file and UI behaviour tests

## Contact
Contact the creator and other Emacs users in the Emacs room on the Jabber network - [xmpp:emacs@salas.suchat.org?join](xmpp:emacs@salas.suchat.org?join)
(For help in getting started with Jabber, [click here](https://xmpp.org/getting-started/))

## License
timeclock-ui is released under your choice of [CC0](https://creativecommons.org/publicdomain/zero/1.0/), [Unlicense](https://unlicense.org/), and [WTFPL](http://www.wtfpl.net/).

## Thanks
wasamasa and #emacs for all their help and support

jwiegley for timeclock.el
