# chronometrist
A time tracker in Emacs with a nice interface

Largely modelled after the Android application, [A Time Tracker](https://github.com/netmackan/ATimeTracker)

* Benefits
  1. Extremely simple and efficient to use
  2. Displays useful information about your time usage
  3. Support for both mouse and keyboard
  4. Human errors in tracking are easily fixed by editing a plain text file
  5. Hooks to let you perform arbitrary actions when starting/stopping tasks

* Limitations
  1. No support (yet) for adding a task without clocking into it.
  2. No support for concurrent tasks.

**WARNING: with the next version (v0.3), chronometrist will no longer use timeclock as a dependency and will use its own s-expression-based backend. A command to migrate the timeclock-file will be provided.**

## Differences from timeclock
* Stores data in an s-expression format rather than a line-based one
* Supports attaching tags and arbitrary key-values to time intervals
* Has commands to shows useful summaries
* Has a more useful implementation of hooks (see [Hooks](#Hooks))

## Installation
You can get `chronometrist` from https://framagit.org/contrapunctus/chronometrist/

`chronometrist` requires [dash.el](https://github.com/magnars/dash.el)

Add the Chronometrist directory to your load-path, and `(require 'chronometrist)`.

## Commands
### chronometrist
Run `M-x chronometrist` to see your projects, the time you spent on them today, which one is active, and the total time clocked today.

Hit `RET` on a project to clock in for it. If it's already clocked in, it will be clocked out, and you'll be prompted for an optional reason. Use `M-RET` if you don't want to be asked for a reason.

Whenever you call `chronometrist`, the cursor will helpfully be placed on the last activity you clocked out of, or the current activity clocked in.

Alternatively, hit `<numeric prefix> RET` anywhere in the buffer to toggle the corresponding project, e.g. `C-1 RET` will toggle the project with index 1.

Press `l` to view your `chronometrist-file` (`~/.emacs.d/chronometrist.sexp` by default). Press `r` to see a weekly report (see `chronometrist-report`)

Running `M-x chronometrist` when the Chronometrist buffer is visible will kill it, so the key you bind it to can function as a toggle.

`chronometrist` keeps itself updated via an idle timer - no need to frequently press `g` to update.

#### Attaching key values to time intervals
Add `chronometrist-kv-read` to `chronometrist-before-in-functions` and/or `chronometrist-before-out-functions`, as you like (see [Hooks](#Hooks)).

You will now be prompted to enter key-values when you clock in/out. Leave an entry blank to exit the prompt, edit the resulting key-values by hand if required, then press `C-c C-c` to accept the key-values (or `C-c C-k` to cancel).

### chronometrist-report
Run `M-x chronometrist-report` (or `chronometrist` with a prefix argument of 1, or press `r` in the `chronometrist` buffer) to see a weekly report.

Press `b` to look at past weeks, and `f` for future weeks.

Press `l` to view your `chronometrist-file`, `~/.emacs.d/chronometrist.sexp` by default.

Just like `chronometrist`, `chronometrist-report` will also toggle the visibilty of the buffer.

`chronometrist-report` keeps itself updated via an idle timer - no pressing `g` to update.

### chronometrist-statistics
Run `M-x chronometrist-statistics` (or `chronometrist` with a prefix argument of 2) to view statistics.

Press `b` to look at past time ranges, and `f` for future ones.

Press `l` to view your `chronometrist-file`, `~/.emacs.d/chronometrist.sexp` by default.

Just like `chronometrist`, `chronometrist-statistics` will also toggle the visibilty of the buffer.

## Customization
See the Customize groups `chronometrist` and `chronometrist-report` for variables intended to be user-customizable.

If you find that you usually _don't_ want to enter a reason, you can switch the default bindings -

```elisp
(define-key chronometrist-mode-map (kbd "M-RET") #'chronometrist-toggle-project)
(define-key chronometrist-mode-map (kbd "RET")   #'chronometrist-toggle-project-no-reason)
```

### Hooks
Chronometrist currently has three hooks -
1. `chronometrist-before-in-functions`
1. `chronometrist-after-in-functions`
2. `chronometrist-before-out-functions`
3. `chronometrist-after-out-functions`

As their names suggest, these are 'abnormal' hooks, i.e. the functions they contain must accept arguments. In this case, each function must accept exactly one argument, which is the project which is being started or stopped.

As an example from the author's own init -

```elisp
(defun my-start-guitar (project)
  (when (equal project "Guitar")
    (find-file-other-window "~/repertoire.org")))

(add-hook 'chronometrist-before-in-functions 'my-start-guitar)
```

Another one, prompting the user if they have uncommitted changes in a git repository (assuming they use [Magit](https://magit.vc/)) -

```elisp
(autoload 'magit-anything-modified-p "magit")

(defun my-commit-prompt ()
  (if (magit-anything-modified-p)
      (if (yes-or-no-p "You have uncommitted changes. Really clock out? ")
          t
        (magit-status)
        nil)
        t))

(add-hook 'chronometrist-before-out-functions 'my-commit-prompt)
```

## Roadmap/Ideas
* Show details for time spent on a project when clicking on a non-zero "time spent" field (in both Chronometrist and Chronometrist-Report buffers).

### chronometrist
1. Make clocked-in project row bold, either in addition to the star, or replacing it.
   - Another activity-indication enhancement - show the current time interval being recorded instead of the star.
2. **Custom day start/end time** - option to use a specific time to define when a day starts/ends. e.g. 08:00 will mean a day starts and ends at 08:00 instead of the usual 24:00/00:00. Helpful for late sleepers.
3. Suggest reasons by frequency? So your most-used reason for the task is the default suggestion. If you usually _don't_ provide a reason for the task, the default is nil.
4. **Better shortcuts** - shortcuts derived from the first alphabet of each project might be nicer.
5. **Modeline support** - show currently active project + time spent on it so far in the mode-line (see timeclock-mode-line-display)
   - Maybe make modeline slowly change color the longer you do something?
6. **Reminder notifications** - a common issue with time trackers is that people forget to clock in/out. A potential solution can be to have Emacs remind people (ideally via desktop notifications?) -
   * when they haven't clocked in, every X minutes (e.g. 30)
   * that they are clocked in, every X minutes (e.g. 30)
   * of course, modeline support might help too.
   * a user-supplied alist of regular expressions/globs matching a file path and projects could be used to offer that the corresponding project be started for them. e.g. given this alist -
     ```
     (("*my-compositions*" . "Composition")
      ("*.el*" . "Programming")
      ("*.scm" . "Programming"))
     ```
     ...when I open any file in `~/my-music/my-compositions/`, I'd be offered to start the "Composition" task. When I open an Emacs Lisp or Scheme file, I'd be asked if I want to start the "Programming" task. (with a variable to not ask and just switch, informing the user when that happens.)
     + Could also add comments based on the path/extension.
     + Instead of a path, could also be the name of a major mode.
     + See `buffer-list-update-hook` and `before-change-functions`
     + an alternative form could look like this -
       ```
       ((PROJECT :pattern ... :mode ... :computerp) ...)
       ```
       - :pattern - glob pattern to match paths
       - :mode - regular expression to match buffer modes
       - :computerp or :emacsp - t if this project (activity) is something you do on a computer/in Emacs (or perhaps, more specifically, the same computer/Emacs instance as the one you run Chronometrist on.). Somewhat implied by the previous arguments. If this is t, Chronometrist will note if the computer has received no events for some time, and clock out of the project. If it's an integer, clock out after that many seconds of computer inactivity.
7. Use `make-thread` in v26 or the emacs-async library for `chronometrist-entries`/`chronometrist-report-entries`
8. Some way to update buffers every second without making Emacs unusable. (impossible?)
9. "Day summary" - for users who use the "reason" feature to note the specifics of their actual work. Combine the reasons together to create a descriptive overview of the work done in the day.
10. Tree of tasks (i.e. sub-tasks etc) - choosing a child task implies the parent tasks too.
    - Alternatively - each task can have multiple tags. See [doc/tags.md](doc/tags.md)
11. Set goal time for one or more tasks.

### Chronometrist-report
1. Show week counter and max weeks; don't scroll past first/last weeks
2. Highlight column of current day
3. Add support for other locale weeks/weekday names
4. Show only certain projects

### chronometrist-statistics
1. Show range counter and max ranges; don't scroll past first/last time ranges
2. activity-specific - average time spent in $TIMEPERIOD, average days worked on in $TIMEPERIOD, current/longest streaks, % of total time in $TIMEPERIOD, % of active time in $TIMEPERIOD, ...
3. general - most productive $TIMEPERIOD, GitHub-style work heatmap calendar, ...
4. press 1 for weekly stats, 2 for monthly, 3 for yearly

### Miscellaneous
1. README - add images
2. [-] Document API somewhere (list of functions, their argument lists, and outputs)
3. [-] Create test timelog file and UI behaviour tests
4. Use for chronometrist-report-weekday-number-alist whatever variables like initial-frame-alist use to get that fancy Custom UI for alists.
5. Multi-timelog-file support?
6. [inflatable raptor](https://github.com/MichaelMure/git-bug/#planned-features)

## Contributions and contact
Feedback and MRs very welcome. 🙂 [doc/hacking.md](doc/hacking.md) contains an introduction to the codebase.

Contact the creator and other Emacs users in the Emacs room on the Jabber network - [xmpp:emacs@salas.suchat.org?join](xmpp:emacs@salas.suchat.org?join) ([web chat](https://inverse.chat/#converse/room?jid=emacs@salas.suchat.org))

(For help in getting started with Jabber, [click here](https://xmpp.org/getting-started/))

## License
Chronometrist is released under your choice of [Unlicense](https://unlicense.org/) and the [WTFPL](http://www.wtfpl.net/).

(See files [LICENSE](LICENSE) and [LICENSE.1](LICENSE.1)).

## Thanks
wasamasa, bpalmer and #emacs for all their help and support

jwiegley for timeclock.el, which we used as a backend in earlier versions

blandest for helping me with the name

fiete for testing and bug reports
