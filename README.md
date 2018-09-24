# chronometrist
A time tracker with a nice interface, using Emacs' timeclock.el

Largely modelled after the Android application, [A Time Tracker](https://github.com/netmackan/ATimeTracker)

## Installation
`chronometrist` requires [dash.el](https://github.com/magnars/dash.el)

Add the Chronometrist directory to your load-path, and `(require 'chronometrist)`.

## Commands
### chronometrist
Run `M-x chronometrist` to see your projects, the time you spent on them today, which one is active, and the total time clocked today.

Hit `RET` on a project to clock in for it. If it's already clocked in, it will be clocked out (and you'll be prompted for an optional reason).

Whenever you call `chronometrist`, the cursor will helpfully be placed on the last activity you clocked out of, or the current activity clocked in.

Alternatively, hit `<numeric prefix> RET` anywhere in the buffer to toggle the corresponding project, e.g. `C-1 RET` will toggle the project with index 1.

Press `l` to view your `timeclock-file` (`~/.emacs.d/timelog` by default). Press `r` to see a weekly report (see `chronometrist-report`)

Running `M-x chronometrist` when the Chronometrist buffer is visible will kill it, so the key you bind it to can function as a toggle.

`chronometrist` keeps itself updated via an idle timer - no pressing `g` to update.

### chronometrist-report
Run `M-x chronometrist-report` (or `chronometrist` with a prefix argument, or press `r` in the `chronometrist` buffer) to see a weekly report.

Press `b` to look at past weeks, and `f` for future weeks.

Press `l` to view your `timeclock-file`, `~/.emacs.d/timelog` by default.

Just like `chronometrist`, `chronometrist-report` will also toggle the visibilty of the buffer.

`chronometrist-report` keeps itself updated via an idle timer - no pressing `g` to update.

## Customization
See the Customize groups `chronometrist` and `chronometrist-report` for variables intended to be user-customizable.

## Roadmap
* New commands for statistics
  - activity-specific - average time spent in $time_period, average days worked on in $time_period, current/longest streaks, ...
  - general - most productive $time_period, GitHub-style work heatmap calendar, ...
* Update when the timeclock-file changes using a filesystem watcher
* Mouse support
* [inflatable raptor](https://github.com/MichaelMure/git-bug/#planned-features)

### chronometrist
1. Add variable to let user control prompting-for-reason behaviour
2. Make clocked-in project row bold, either in addition to the star, or replacing it.
3. **'kill'/discard command** (bind to 'k'), which will delete the interval currently being recorded.
   - Most conservative option - it will only operate on the project at point, and will only kill for a clocked-in project.
   - Somewhat less conservative option - it will operate on the currently clocked-in project, no matter where point is.
   - It _should_ ask for confirmation.
4. **Custom day start/end time** - option to use a specific time to define when a day starts/ends. e.g. 08:00 will mean a day starts and ends at 08:00 instead of the usual 24:00/00:00. Helpful for late sleepers.
5. **Fix suggested reasons** - the default reason suggested is the last one used. Can't even begin to explain how nonsensical that is. (might be an ido or timeclock.el problem)
   - Make default blank?
   - Possibly make suggestions activity-sensitive e.g. when stopping activity A, don't suggest past reasons used for activity B, C, etc. Add a variable to customize this, because it might not be the behaviour everyone wants.
   - Suggest reasons by frequency? So your most used reason for the task is the default suggestion. If you usually _don't_ provide a reason for the task, the default is nil.
6. **Better shortcuts** - Shortcuts derived from the first alphabet of each project could be even nicer.
7. **Modeline support** - show currently active project + time spent on it so far in the mode-line (see timeclock-mode-line-display)
   - Maybe make modeline slowly change color the longer you do something?
8. **Improve help**
   - Show shortcuts by consulting the keymap rather than using a hardcoded string.
   - Change 'see weekly report' and 'open log file' to buttons
9. **Reminder notifications** - a common issue with time trackers is that people forget to clock in/out. A potential solution can be to have Emacs remind people (ideally via desktop notifications?) -
    - when they haven't clocked in, every X minutes (e.g. 30)
    - that they are clocked in, every X minutes (e.g. 30)
    - of course, modeline support might help too.
10. Use `make-thread` in v26 or the emacs-async library for `chronometrist-entries`/`chronometrist-report-entries`
11. Some way to update buffers every second without making Emacs unusable. (impossible?)
12. "Day summary" - for users who use the "reason" feature to note the specifics of their actual work. Combine the reasons together to create a descriptive overview of the work done in the day.
13. Commands to rename a project, delete a project (erasing all records)

### chronometrist-report
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
Chronometrist is released under your choice of [CC0](https://creativecommons.org/publicdomain/zero/1.0/), [Unlicense](https://unlicense.org/), and [WTFPL](http://www.wtfpl.net/).

(See files [LICENSE](LICENSE), [LICENSE.1](LICENSE.1), and [LICENSE.2](LICENSE.2))

## Thanks
wasamasa and #emacs for all their help and support

jwiegley for timeclock.el

blandest for helping me with the name

fiete for testing
