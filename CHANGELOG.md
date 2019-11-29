# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]
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
