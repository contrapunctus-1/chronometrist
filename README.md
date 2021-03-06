[![MELPA](https://melpa.org/packages/chronometrist-badge.svg)](https://melpa.org/#/chronometrist)

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

**IMPORTANT: with version v0.3, chronometrist no longer uses timeclock as a dependency and will use its own s-expression-based backend. A command to migrate the timeclock-file, `chronometrist-migrate-timelog-file->sexp-file`, is provided.**

## Comparisons
### timeclock.el
Compared to timeclock.el, Chronometrist
* stores data in an s-expression format rather than a line-based one
* supports attaching tags and arbitrary key-values to time intervals
* has commands to shows useful summaries
* has more hooks (see [Hooks](#Hooks))

### Org time tracking
Chronometrist and Org time tracking seem to be equivalent in terms of capabilities, approaching the same ends through different means.
* Chronometrist doesn't have a mode line indicator at the moment. (planned)
* Chronometrist doesn't have Org's sophisticated querying facilities. (an SQLite backend is planned)
* Org does so many things that keybindings seem to necessarily get longer. Chronometrist has far fewer commands than Org, so most of the keybindings are single keys, without modifiers.
* Chronometrist's UI makes keybindings discoverable - they are displayed in the buffers themselves.
* Chronometrist's UI is cleaner, since the storage is separate from the display. It doesn't show tasks as trees like Org, but it uses tags and key-values to achieve that. Additionally, navigating a flat list takes fewer user operations than navigating a tree.
* Chronometrist data is just s-expressions (plists), and may be easier to parse than a complex text format with numerous use-cases.

## Installation
### from MELPA
1. Set up MELPA - https://melpa.org/#/getting-started

   (Chronometrist uses semantic versioning and only releases are pushed to the master branch, so using MELPA Stable is recommended and has no effect on frequency of updates.)
2. `M-x package-install RET chronometrist RET`

### from Git
You can get `chronometrist` from https://github.com/contrapunctus-1/chronometrist

`chronometrist` requires
* Emacs v25 or higher
* [dash.el](https://github.com/magnars/dash.el)
* [ts.el](https://github.com/alphapapa/ts.el)

The optional extension `chronometrist-key-values` requires `choice.el`, apart from `chronometrist` itself.

Add the "elisp/" subdirectory to your load-path, and `(require 'chronometrist)`.

## Usage
In the buffers created by the following three commands, you can press `l` (`chronometrist-open-log`) to view/edit your `chronometrist-file`, which by default is `~/.emacs.d/chronometrist.sexp`.

All of these commands will kill their buffer when run again with the buffer visible, so the keys you bind them to behave as a toggle.

### chronometrist
Run `M-x chronometrist` to see your projects, the time you spent on them today, which one is active, and the total time clocked today.

Hit `RET` (`chronometrist-toggle-task`) on a project to start tracking time for it. If it's already clocked in, it will be clocked out. This command runs some [hooks](#Hooks), which are useful for a wide range of functionality (see [Adding more information](#adding-more-information-experimental) below). In some cases, you may want to skip running the hooks - use `M-RET` (`chronometrist-toggle-task-no-hooks`) to do that.

You can also hit `<numeric prefix> RET` anywhere in the buffer to toggle the corresponding project, e.g. `C-1 RET` will toggle the project with index 1.

Press `r` to see a weekly report (see `chronometrist-report`)

`chronometrist` keeps itself updated via an idle timer - no need to frequently press `g` to update.

### chronometrist-report
Run `M-x chronometrist-report` (or `chronometrist` with a prefix argument of 1, or press `r` in the `chronometrist` buffer) to see a weekly report.

Press `b` to look at past weeks, and `f` for future weeks.

`chronometrist-report` keeps itself updated via an idle timer - no pressing `g` to update.

### chronometrist-statistics
Run `M-x chronometrist-statistics` (or `chronometrist` with a prefix argument of 2) to view statistics.

Press `b` to look at past time ranges, and `f` for future ones.

### Attaching tags and key values
Part of the reason Chronometrist stores time intervals as property lists is to allow you to add tags and arbitrary key-values to them.

#### Tags
To be prompted for tags, add `chronometrist-tags-add` to any hook except `chronometrist-before-in-functions`, based on your preference (see [Hooks](#Hooks)). The prompt suggests past combinations you used for the current task, which you can browse with `M-p`/`M-n`. You can leave it blank by pressing `RET`, or skip the prompt just this once by pressing `M-RET` (`chronometrist-toggle-task-no-hooks`).

#### Key-value pairs
Similarly, to be prompted for key-values, add `chronometrist-kv-add` to any hook except `chronometrist-before-in-functions`. To exit the prompt, press the key it indicates for quitting - you can then edit the resulting key-values by hand if required. Press `C-c C-c` to accept the key-values, or `C-c C-k` to cancel.

### Prompt when exiting Emacs
If you wish to be prompted when you exit Emacs while tracking time, you can use this -

`(add-hook 'kill-emacs-query-functions 'chronometrist-query-stop)`

### Time goals/targets
If you wish you could define time goals for some tasks, and have Chronometrist notify you when you're approaching the goal, completing it, or exceeding it, check out the extension [chronometrist-goal.el](https://github.com/contrapunctus-1/chronometrist-goal/).

## Customization
See the Customize groups `chronometrist` and `chronometrist-report` for variables intended to be user-customizable.

### Hooks
Chronometrist currently has the following hooks -
1. `chronometrist-mode-hook`
2. `chronometrist-before-in-functions`
3. `chronometrist-after-in-functions`
4. `chronometrist-before-out-functions`
5. `chronometrist-after-out-functions`
6. `chronometrist-list-format-transformers`
7. `chronometrist-entry-transformers`
8. `chronometrist-file-change-hook`

The hooks whose names end with `-functions` are abnormal hooks - each function must accept exactly one argument, which is the name of the project which is being started or stopped, as a string.

`chronometrist-before-out-functions` is different from the other three, in that it runs until failure - the task will be clocked out only if all functions in this hook return `t`.

### Opening certain files when you start a task
An idea from the author's own init -

```elisp
(defun my-start-project (project)
  (pcase project
    ("Guitar"
     (find-file-other-window "~/repertoire.org"))
    ;; ...
    ))

(add-hook 'chronometrist-before-in-functions 'my-start-project)
```

### Reminding you to commit your changes
Another one, prompting the user if they have uncommitted changes in a git repository (assuming they use [Magit](https://magit.vc/)) -

```elisp
(autoload 'magit-anything-modified-p "magit")

(defun my-commit-prompt ()
  "Prompt user if `default-directory' is a dirty Git repository.
Return t if the user answers yes, if the repository is clean, or
if there is no Git repository.

Return nil (and run `magit-status') if the user answers no."
  (cond ((not (magit-anything-modified-p)) t)
        ((yes-or-no-p
          (format "You have uncommitted changes in %S. Really clock out? "
                  default-directory)) t)
        (t (magit-status) nil)))

(add-hook 'chronometrist-before-out-functions 'my-commit-prompt)
```

### Displaying the current time interval in the activity indicator
```elisp
(defun my-activity-indicator ()
  (thread-last (plist-put (chronometrist-last)
                          :stop (chronometrist-format-time-iso8601))
    list
    chronometrist-events-to-durations
    (-reduce #'+)
    truncate
    chronometrist-format-time))

(setq chronometrist-activity-indicator #'my-activity-indicator)
```

## Roadmap/Ideas
* Show details for time spent on a project when clicking on a non-zero "time spent" field (in both Chronometrist and Chronometrist-Report buffers).

### chronometrist
1. Use `make-thread` in v26 or the emacs-async library for `chronometrist-entries`/`chronometrist-report-entries`
2. Some way to update buffers every second without making Emacs unusable. (impossible?)
3. "Day summary" - for users who use the "reason" feature to note the specifics of their actual work. Combine the reasons together to create a descriptive overview of the work done in the day.

### chronometrist-statistics
1. Show range counter and max ranges; don't scroll past first/last time ranges
2. activity-specific - average time spent in $TIMEPERIOD, average days worked on in $TIMEPERIOD, current/longest/last streak, % of $TIMEPERIOD, % of active (tracked) time in $TIMEPERIOD, ...
3. general - most productive $TIMEPERIOD, GitHub-style work heatmap calendar, ...
4. press 1 for weekly stats, 2 for monthly, 3 for yearly

### Miscellaneous
1. README - add images
2. [-] Create test timelog file and UI behaviour tests
3. Use for `chronometrist-report-weekday-number-alist` whatever variables like `initial-frame-alist` use to get that fancy Custom UI for alists.
4. Multi-timelog-file support?
5. [inflatable raptor](https://github.com/MichaelMure/git-bug/#planned-features)

## Contributions and contact
Feedback and MRs are very welcome. 🙂
* [TODO.org](TODO.org) has a long list of tasks
* [doc/manual.org](doc/manual.org) contains an overview of the codebase, explains various mechanisms and decisions, and has a reference of definitions.

If you have tried using Chronometrist, I'd love to hear your experiences! Get in touch with the author and other Emacs users in the Emacs channel on the Jabber network - [xmpp:emacs@salas.suchat.org?join](https://conversations.im/j/emacs@salas.suchat.org) ([web chat](https://inverse.chat/#converse/room?jid=emacs@salas.suchat.org))

(For help in getting started with Jabber, [click here](https://xmpp.org/getting-started/))

## License
I dream of a world where all software is liberated - transparent, trustable, and accessible for anyone to use or improve. But I don't want to make demands or threats (e.g. via legal conditions) to get there.

I'd rather make a request - please do everything you can to help that dream come true. Please Unlicense as much software as you can.

Chronometrist is released under your choice of [Unlicense](https://unlicense.org/) or the [WTFPL](http://www.wtfpl.net/).

(See files [UNLICENSE](UNLICENSE) and [WTFPL](WTFPL)).

## Thanks
wasamasa, bpalmer, aidalgol, pjb and the rest of #emacs for their tireless help and support

jwiegley for timeclock.el, which we used as a backend in earlier versions

blandest for helping me with the name

fiete and wu-lee for testing and bug reports
