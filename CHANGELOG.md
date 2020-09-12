# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [unreleased]
### Added
* New hooks - `chronometrist-mode-hook`, `chronometrist-list-format-transformers`, `chronometrist-entry-transformers`. chronometrist no longer needs to know about extensions.
* New custom variable `chronometrist-sexp-pretty-print-function`
### Fixed
* Remove quotes from key-value prompt in quit keybindings
* Lisp objects being stored as un`read`able strings in `chronometrist-value-history`, resulting in value suggestions not matching user input.

## [0.5.5] - 2020-09-02
### Added
* `chronometrist-skip-query-prompt` to re-use last-used tags/key-values with a single key. (...assuming you use `y-or-n-p`)
### Changed
* Prompts for keys and values now use `completing-read`, making the interface and the controls more consistent.

## [0.5.4] - 2020-07-19
### Fixed
* Bug resulting in only the last tag combination being suggested

## [0.5.3] - 2020-07-06
### Changed
* `chronometrist-goals` has been renamed to `chronometrist-goal`

## [0.5.2] - 2020-07-05
### Fixed
* Package long description in the package menu

## [0.5.1] - 2020-06-30
### Fixed
* Error when adding task (trying to append an atom to a list)

## [0.5.0] - 2020-06-30
### Added
* Support for time goals via optional package `chronometrist-targets`.
* New hook - `chronometrist-file-change-hook`
### Changed
* Use [ts.el](https://github.com/alphapapa/ts.el) structs to represent date-time, wherever possible. (`chronometrist-events` and `chronometrist-file` being notable exceptions)
### Fixed
* Prefix arguments now work with the point on a button, too.
* Bug with missing entries in `chronometrist-key-history`
* Operations for adding a new s-expression and replacing the last s-expression have been optimized. Notably, commands for clocking in/out are now significantly faster.

## [0.4.4]
### Fixed
* Error when adding a task for the first time.

## [0.4.3] - 2020-05-03
### Changed
* `chronometrist-toggle-task-no-reason` (which did nothing since the migration from timeclock.el) is now called `chronometrist-toggle-task-no-hooks`. It will toggle the task without running the before-in/after-in/before-out/after-out functions.
### Fixed
* Refresh buffer when clocking in (instead of waiting for first timer refresh)
* Insertion of values is now slightly smarter about detecting and handling Lisp data types.

## [0.4.2] - 2020-01-15
### Fixed
* Library headers for MELPA release

## [0.4.1] - 2020-01-12
### Fixed
* Various declarations for MELPA release

## [0.4.0] - 2019-11-29
### Added
* Custom variable `chronometrist-activity-indicator` to change how an active task is indicated.
* `chronometrist-query-stop` for prompting on exiting Emacs
### Fixed
* `chronometrist-kv-accept` will not modify the file if there are no key-values.
* Regression in `chronometrist-value-history-populate`
* Migrate `chronometrist-statistics` to new format

## [0.3.2] - 2019-11-23
### Fixed
* Regression in `chronometrist-value-history-populate`

## [0.3.1] - 2019-11-22
### Fixed
* Improved load time via code cleanup + inhibiting `chronometrist-events-populate` for task start/stop

### Removed
1. Deprecated functions
2. Leftover pre-v0.3 variables

## [0.3.0] - 2019-10-31
### Added
* s-expression file format support
* functions to read arbitrary key-values (see `chronometrist-kv-add`, `chronometrist-kv-accept`, `chronometrist-kv-reject`)
* hook `chronometrist-after-in-functions`

### Changed
* hooks are now called `chronometrist-before-in-functions`, `chronometrist-before-out-functions`, and `chronometrist-after-out-functions`

### Removed
* timeclock.el file format support

### Deprecated
1. `chronometrist-timestamp->seconds`
2. `chronometrist-timestamp-list->seconds`
3. `chronometrist-time-re-file`
4. `chronometrist-get-end-time`
5. `chronometrist-date-op-internal`
6. `chronometrist-reason-list`
7. `chronometrist-ask-for-reason`

## [0.2.2] - 2019-09-09
### Fixed
* Error resulting from incorrect variable name in chronometrist-maybe-stop-timer
* Long waiting times after saving timeclock-file due to multiple, erroneously-created file system watchers.

### Deprecated
* timeclock will be removed as a backend in the next release.

## [0.2.1] - 2019-08-14
### Fixed
* bug with wrongly named function in chronometrist-report

## [0.2.0] - 2019-08-09
### Added
* Autoload cookies
* chronometrist-before-project-stop-functions hook

### Fixed
* Try to remove unnecessary file re-reading
* Buffer refresh bugs

## [0.1.0] - 2019-08-05
