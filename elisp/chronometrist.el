;;; chronometrist.el --- A time tracker with a nice interface -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabber.fr>
;; Maintainer: contrapunctus <xmpp:contrapunctus@jabber.fr>
;; Keywords: calendar
;; Homepage: https://github.com/contrapunctus-1/chronometrist
;; Package-Requires: ((emacs "25.1") (literate-elisp "0.1") (dash "2.16.0") (seq "2.20") (ts "0.2"))
;; Version: 0.7.0

;; This is free and unencumbered software released into the public domain.
;;
;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.
;;
;; For more information, please refer to <https://unlicense.org>

;;; Commentary:
;;
;; A time tracker in Emacs with a nice interface

;; Largely modelled after the Android application, [A Time Tracker](https://github.com/netmackan/ATimeTracker)

;; * Benefits
;;   1. Extremely simple and efficient to use
;;   2. Displays useful information about your time usage
;;   3. Support for both mouse and keyboard
;;   4. Human errors in tracking are easily fixed by editing a plain text file
;;   5. Hooks to let you perform arbitrary actions when starting/stopping tasks

;; * Limitations
;;   1. No support (yet) for adding a task without clocking into it.
;;   2. No support for concurrent tasks.

;; ## Comparisons
;; ### timeclock.el
;; Compared to timeclock.el, Chronometrist
;; * stores data in an s-expression format rather than a line-based one
;; * supports attaching tags and arbitrary key-values to time intervals
;; * has commands to shows useful summaries
;; * has more hooks

;; ### Org time tracking
;; Chronometrist and Org time tracking seem to be equivalent in terms of capabilities, approaching the same ends through different means.
;; * Chronometrist doesn't have a mode line indicator at the moment. (planned)
;; * Chronometrist doesn't have Org's sophisticated querying facilities. (an SQLite backend is planned)
;; * Org does so many things that keybindings seem to necessarily get longer. Chronometrist has far fewer commands than Org, so most of the keybindings are single keys, without modifiers.
;; * Chronometrist's UI makes keybindings discoverable - they are displayed in the buffers themselves.
;; * Chronometrist's UI is cleaner, since the storage is separate from the display. It doesn't show tasks as trees like Org, but it uses tags and key-values to achieve that. Additionally, navigating a flat list takes fewer user operations than navigating a tree.
;; * Chronometrist data is just s-expressions (plists), and may be easier to parse than a complex text format with numerous use-cases.

;; For information on usage and customization, see https://github.com/contrapunctus-1/chronometrist/blob/master/README.md

;;; Code:
(require 'literate-elisp)

(literate-elisp-load
 (format "%schronometrist.org" (file-name-directory load-file-name)))

(provide 'chronometrist)

;;; chronometrist.el ends here
