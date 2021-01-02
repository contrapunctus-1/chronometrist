;;; chronometrist.el --- A time tracker with a nice interface -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabber.fr>
;; Maintainer: contrapunctus <xmpp:contrapunctus@jabber.fr>
;; Keywords: calendar
;; Homepage: https://github.com/contrapunctus-1/chronometrist
;; Package-Requires: ((emacs "25.1")
;;                    (dash "2.16.0")
;;                    (seq "2.20")
;;                    (s "1.12.0")
;;                    (ts "0.2")
;;                    (anaphora "1.0.4")
;;                    (run-transformers "0.0.1"))
;; Version: 0.5.6

(require 'filenotify)
(require 'cl-lib)
(require 'subr-x)
(require 'run-transformers)

(require 'chronometrist-common)
(require 'chronometrist-key-values)
(require 'chronometrist-queries)
(require 'chronometrist-migrate)
(require 'chronometrist-sexp)

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
(eval-when-compile
  (defvar chronometrist-mode-map)
  (require 'subr-x))
(autoload 'chronometrist-maybe-start-timer "chronometrist-timer" nil t)
(autoload 'chronometrist-report "chronometrist-report" nil t)
(autoload 'chronometrist-statistics "chronometrist-statistics" nil t)

;; ## VARIABLES ##

(defgroup chronometrist nil
  "A time tracker with a nice UI."
  :group 'applications)

(defcustom chronometrist-file
  (locate-user-emacs-file "chronometrist.sexp")
  "Default path and name of the Chronometrist database.

It should be a text file containing plists in the form -
\(:name \"task name\"
 [:tags TAGS]
 [:comment \"comment\"]
 [KEY-VALUE-PAIR ...]
 :start \"TIME\"
 :stop \"TIME\"\)

Where -

TAGS is a list. It can contain any strings and symbols.

KEY-VALUE-PAIR can be any keyword-value pairs. Currently,
Chronometrist ignores them.

TIME must be an ISO-8601 time string.

\(The square brackets here refer to optional elements, not
vectors.\)"
  :type 'file)

(defcustom chronometrist-buffer-name "*Chronometrist*"
  "The name of the buffer created by `chronometrist'."
  :type 'string)

(defcustom chronometrist-hide-cursor nil
  "If non-nil, hide the cursor and only highlight the current line in the `chronometrist' buffer."
  :type 'boolean)

(defcustom chronometrist-update-interval 5
  "How often the `chronometrist' buffer should be updated, in seconds.

This is not guaranteed to be accurate - see (info \"(elisp)Timers\")."
  :type 'integer)

(defcustom chronometrist-activity-indicator "*"
  "How to indicate that a task is active.
Can be a string to be displayed, or a function which returns this string.
The default is \"*\""
  :type '(choice string function))

(defcustom chronometrist-day-start-time "00:00:00"
  "The time at which a day is considered to start, in \"HH:MM:SS\".

The default is midnight, i.e. \"00:00:00\"."
  :type 'string)

(defvar chronometrist--task-history nil)
(defvar chronometrist--point nil)

;; ## FUNCTIONS ##
(defun chronometrist-open-log (&optional _button)
  "Open `chronometrist-file' in another window.

Argument _BUTTON is for the purpose of using this command as a
button action."
  (interactive)
  (chronometrist-sexp-open-log))

(defun chronometrist-common-create-file ()
  "Create `chronometrist-file' if it doesn't already exist."
  (chronometrist-sexp-create-file))

(defun chronometrist-task-active? (task)
  "Return t if TASK is currently clocked in, else nil."
  (equal (chronometrist-current-task) task))

(defun chronometrist-activity-indicator ()
  "Return a string to indicate that a task is active.
See custom variable `chronometrist-activity-indicator'."
  (if (functionp chronometrist-activity-indicator)
      (funcall chronometrist-activity-indicator)
    chronometrist-activity-indicator))

(defun chronometrist-entries ()
  "Create entries to be displayed in the buffer created by `chronometrist', in the format specified by `tabulated-list-entries'."
  ;; HACK - these calls are commented out, because `chronometrist-entries' is
  ;; called by both `chronometrist-refresh' and `chronometrist-refresh-file', and only the
  ;; latter should refresh from a file.
  ;; (chronometrist-events-populate)
  ;; (chronometrist-events-clean)
  (->> (-sort #'string-lessp chronometrist-task-list)
       (--map-indexed
        (let* ((task        it)
               (index       (number-to-string (1+ it-index)))
               (task-button `(,task action chronometrist-toggle-task-button follow-link t))
               (task-time   (chronometrist-format-time (chronometrist-task-time-one-day task)))
               (indicator   (if (chronometrist-task-active? task) (chronometrist-activity-indicator) "")))
          (--> (vector index task-button task-time indicator)
               (list task it)
               (run-transformers chronometrist-entry-transformers it))))))

(defun chronometrist-task-at-point ()
  "Return the task at point in the `chronometrist' buffer, or nil if there is no task at point."
  (save-excursion
    (beginning-of-line)
    (when (re-search-forward "[0-9]+ +" nil t)
      (get-text-property (point) 'tabulated-list-id))))

(defun chronometrist-goto-last-task ()
  "In the `chronometrist' buffer, move point to the line containing the last active task."
  (goto-char (point-min))
  (re-search-forward (plist-get (chronometrist-last) :name) nil t)
  (beginning-of-line))

(defun chronometrist-print-keybind (command &optional description firstonly)
  "Insert the keybindings for COMMAND.
If DESCRIPTION is non-nil, insert that too.
If FIRSTONLY is non-nil, return only the first keybinding found."
  (insert
   (format "\n% 18s - %s"
           (chronometrist-format-keybinds command chronometrist-mode-map firstonly)
           (if description description ""))))

(defun chronometrist-print-non-tabular ()
  "Print the non-tabular part of the buffer in `chronometrist'."
  (with-current-buffer chronometrist-buffer-name
    (let ((inhibit-read-only t)
          (w "\n    ")
          ;; (keybind-start-new (chronometrist-format-keybinds 'chronometrist-add-new-task chronometrist-mode-map))
          (keybind-toggle    (chronometrist-format-keybinds 'chronometrist-toggle-task chronometrist-mode-map t)))
      (goto-char (point-max))
      (--> (chronometrist-active-time-one-day)
           (chronometrist-format-time it)
           (format "%s%- 26s%s" w "Total" it)
           (insert it))
      (insert "\n")
      (insert w (format "% 17s" "Keys") w (format "% 17s" "----"))
      (chronometrist-print-keybind 'chronometrist-add-new-task)
      (insert-text-button "start a new task" 'action #'chronometrist-add-new-task-button 'follow-link t)
      (chronometrist-print-keybind 'chronometrist-toggle-task "toggle task at point")
      (chronometrist-print-keybind 'chronometrist-toggle-task-no-hooks "toggle without running hooks")
      (insert "\n " (format "%s %s - %s" "<numeric argument N>" keybind-toggle "toggle <N>th task"))
      (chronometrist-print-keybind 'chronometrist-report)
      (insert-text-button "see weekly report" 'action #'chronometrist-report 'follow-link t)
      (chronometrist-print-keybind 'chronometrist-open-log)
      (insert-text-button "view/edit log file" 'action #'chronometrist-open-log 'follow-link t)
      (insert "\n"))))

(defun chronometrist-goto-nth-task (n)
  "Move point to the line containing the Nth task.
Return the task at point, or nil if there is no corresponding
task. N must be a positive integer."
  (goto-char (point-min))
  (when (re-search-forward (format "^%d" n) nil t)
    (beginning-of-line)
    (chronometrist-task-at-point)))

(defun chronometrist-refresh (&optional _ignore-auto _noconfirm)
  "Refresh the `chronometrist' buffer, without re-reading `chronometrist-file'.

The optional arguments _IGNORE-AUTO and _NOCONFIRM are ignored,
and are present solely for the sake of using this function as a
value of `revert-buffer-function'."
  (let* ((window (get-buffer-window chronometrist-buffer-name t))
         (point  (window-point window)))
    (when window
      (with-current-buffer chronometrist-buffer-name
        (tabulated-list-print t nil)
        (chronometrist-print-non-tabular)
        (chronometrist-maybe-start-timer)
        (set-window-point window point)))))

(defvar chronometrist--file-hashes nil
  "List of hashes of `chronometrist-file'.
`chronometrist-refresh-file' sets this to a plist in the form

\(:last HASH-1 :rest HASH-2)

where HASH-1 and HASH-2 are lists in the form

\(START END HASH)

\(see `chronometrist-file-hash')

HASH-1 represents the text between the start of the last
s-expression to the end of the file, and

HASH-2 represents the text between the start of the file and the
position before the last s-expression.")

(defun chronometrist-file-hash (&optional start end)
  "Calculate hash of `chronometrist-file' between START and END.
START can be
a number or marker,
:before-last - the position at the start of the last s-expression
nil or any other value - the value of `point-min'.

END can be
a number or marker,
:before-last - the position at the start of the last s-expression,
nil or any other value - the position at the end of the last s-expression.

Return a list in the form (A B HASH), where A and B are markers
in `chronometrist-file' describing the region for which HASH was calculated."
  (chronometrist-sexp-in-file chronometrist-file
    (let* ((start (cond ((eq :before-last start)
                         (goto-char (point-max))
                         (backward-list))
                        ((number-or-marker-p start) start)
                        (t (point-min))))
           (end   (cond ((eq :before-last end)
                         (goto-char (point-max))
                         (backward-list))
                        ((number-or-marker-p end) end)
                        (t (goto-char (point-max))
                           (backward-list)
                           (forward-list)))))
      (--> (buffer-substring-no-properties start end)
           (secure-hash 'sha1 it)
           (list start end it)))))

;; tests -
;; add newline after last expression and save
;; remove newline afer last expession and save
;; remove a key-value from last expression

(defun chronometrist-file-change-type (hashes)
  "Determine the type of change made to `chronometrist-file'.
HASHES must be a plist. (see `chronometrist--file-hashes')

Return
:append  if a new s-expression was added to the end,
  :last  if the last s-expression was modified,
    nil  if the contents didn't change, and
      t  for any other change."
  (-let* (((last-start last-end last-hash) (plist-get hashes :last))
          ((rest-start rest-end rest-hash) (plist-get hashes :rest))
          ;; Using a hash for the last expression can cause issues -
          ;; the last expression may shrink, and if we try to hash the
          ;; old region again to determine if it has changed, we will
          ;; get an args-out-of-range error.
          (last-same-p   (--> (hash-table-keys chronometrist-events)
                              (last it)
                              (car it)
                              (gethash it chronometrist-events)
                              (last it)
                              (car it)
                              (equal it (chronometrist-last))))
          (new-rest-hash (third (chronometrist-file-hash rest-start rest-end)))
          (rest-same-p   (equal rest-hash new-rest-hash))
          (appended-sexp (chronometrist-sexp-in-file chronometrist-file
                           (goto-char last-end)
                           (ignore-errors (read (current-buffer))))))
    (cond ((not rest-same-p) t)
          (last-same-p (when appended-sexp :append))
          (t :last))))

(defun chronometrist-refresh-file (_fs-event)
  "Re-read `chronometrist-file' and refresh the `chronometrist' buffer.
Argument _FS-EVENT is ignored."
  (run-hooks 'chronometrist-file-change-hook)
  ;; `chronometrist-file-change-type' must be run /before/ we update `chronometrist--file-hashes'
  ;; (the latter represents the old state of the file, which
  ;; `chronometrist-file-change-type' compares with the new one)
  (aif chronometrist--file-hashes
      (let ((file-change-type (chronometrist-file-change-type it)))
        (message "chronometrist - file change type is %s" file-change-type)
        (cond ((eq file-change-type :append)
               (chronometrist-events-add (chronometrist-sexp-last)))
              ((eq file-change-type :last)
               (chronometrist-events-replace-last (chronometrist-sexp-last)))
              ((null file-change-type) nil)
              (t (chronometrist-events-populate))))
    (chronometrist-events-populate)
    (setq chronometrist-task-list
          (-> (chronometrist-map-file chronometrist-file
                (lambda (plist)
                  (plist-get plist :name)))
              (cl-remove-duplicates  :test #'equal)
              (sort  #'string-lessp))))
  (setq chronometrist--file-hashes
        (list :last (chronometrist-file-hash :before-last nil)
              :rest (chronometrist-file-hash nil :before-last)))
  ;; REVIEW - can we move most/all of this to the `chronometrist-file-change-hook'?
  (chronometrist-refresh))

(defun chronometrist-query-stop ()
  "Ask the user if they would like to clock out."
  (let ((task (chronometrist-current-task)))
    (and task
         (yes-or-no-p (format "Stop tracking time for %s? " task))
         (chronometrist-out))
    t))

(defun chronometrist-in (task &optional _prefix)
  "Clock in to TASK; record current time in `chronometrist-file'.
TASK is the name of the task, a string. PREFIX is ignored."
  (interactive "P")
  (let ((plist `(:name ,task :start ,(chronometrist-format-time-iso8601))))
    (chronometrist-sexp-new plist)
    (chronometrist-refresh)))

(defun chronometrist-out (&optional _prefix)
  "Record current moment as stop time to last s-exp in `chronometrist-file'.
PREFIX is ignored."
  (interactive "P")
  (let ((plist (plist-put (chronometrist-last) :stop (chronometrist-format-time-iso8601))))
    (chronometrist-sexp-replace-last plist)))

;; ## HOOKS ##
(defvar chronometrist-mode-hook nil
  "Normal hook run at the very end of `chronometrist-mode'.")

(defvar chronometrist-list-format-transformers nil
  "List of functions to transform `tabulated-list-format' (which see).
This is called with `run-transformers' in `chronometrist-mode', which see.

Extensions using `chronometrist-list-format-transformers' to
increase the number of columns will also need to modify the value
of `tabulated-list-entries' by using
`chronometrist-entry-transformers'.")

(defvar chronometrist-entry-transformers nil
  "List of functions to transform each entry of `tabulated-list-entries'.
This is called with `run-transformers' in `chronometrist-entries', which see.

Extensions using `chronometrist-entry-transformers' to increase
the number of columns will also need to modify the value of
`tabulated-list-format' by using
`chronometrist-list-format-transformers'.")

(defvar chronometrist-before-in-functions nil
  "Functions to run before a task is clocked in.
Each function in this hook must accept a single argument, which
is the name of the task to be clocked-in.

The commands `chronometrist-toggle-task-button',
`chronometrist-add-new-task-button', `chronometrist-toggle-task',
and `chronometrist-add-new-task' will run this hook.")

(defvar chronometrist-after-in-functions nil
  "Functions to run after a task is clocked in.
Each function in this hook must accept a single argument, which
is the name of the task to be clocked-in.

The commands `chronometrist-toggle-task-button',
`chronometrist-add-new-task-button', `chronometrist-toggle-task',
and `chronometrist-add-new-task' will run this hook.")

(defvar chronometrist-before-out-functions nil
  "Functions to run before a task is clocked out.
Each function in this hook must accept a single argument, which
is the name of the task to be clocked out of.

The task will be stopped only if all functions in this list
return a non-nil value.")

(defvar chronometrist-after-out-functions nil
  "Functions to run after a task is clocked out.
Each function in this hook must accept a single argument, which
is the name of the task to be clocked out of.")

(defvar chronometrist-file-change-hook nil
  "Functions to be run after `chronometrist-file' is changed on disk.")

(defun chronometrist-run-functions-and-clock-in (task)
  "Run hooks and clock in to TASK."
  (run-hook-with-args 'chronometrist-before-in-functions task)
  (chronometrist-in task)
  (run-hook-with-args 'chronometrist-after-in-functions task))

(defun chronometrist-run-functions-and-clock-out (task)
  "Run hooks and clock out of TASK."
  (when (run-hook-with-args-until-failure 'chronometrist-before-out-functions task)
    (chronometrist-out)
    (run-hook-with-args 'chronometrist-after-out-functions task)))

;; ## MAJOR-MODE ##
(defvar chronometrist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")   #'chronometrist-toggle-task)
    (define-key map (kbd "M-RET") #'chronometrist-toggle-task-no-hooks)
    (define-key map (kbd "l")     #'chronometrist-open-log)
    (define-key map (kbd "r")     #'chronometrist-report)
    (define-key map [mouse-1]     #'chronometrist-toggle-task)
    (define-key map [mouse-3]     #'chronometrist-toggle-task-no-hooks)
    (define-key map (kbd "a")     #'chronometrist-add-new-task)
    map)
  "Keymap used by `chronometrist-mode'.")

(define-derived-mode chronometrist-mode tabulated-list-mode "Chronometrist"
  "Major mode for `chronometrist'."
  (make-local-variable 'tabulated-list-format)
  (--> [("#" 3 t) ("Task" 25 t) ("Time" 10 t) ("Active" 10 t)]
        (run-transformers chronometrist-list-format-transformers it)
        (setq tabulated-list-format it))
  (make-local-variable 'tabulated-list-entries)
  (setq tabulated-list-entries 'chronometrist-entries)
  (make-local-variable 'tabulated-list-sort-key)
  (setq tabulated-list-sort-key '("Task" . nil))
  (tabulated-list-init-header)
  (setq revert-buffer-function #'chronometrist-refresh)
  (run-hooks 'chronometrist-mode-hook))

;; ## BUTTONS ##

(defun chronometrist-toggle-task-button (_button)
  "Button action to toggle a task.

Argument _BUTTON is for the purpose of using this as a button
action, and is ignored."
  (when current-prefix-arg
    (chronometrist-goto-nth-task (prefix-numeric-value current-prefix-arg)))
  (let ((current  (chronometrist-current-task))
        (at-point (chronometrist-task-at-point)))
    ;; clocked in + point on current    = clock out
    ;; clocked in + point on some other task = clock out, clock in to task
    ;; clocked out = clock in
    (when current
      (chronometrist-run-functions-and-clock-out current))
    (unless (equal at-point current)
      (chronometrist-run-functions-and-clock-in at-point))))

(defun chronometrist-add-new-task-button (_button)
  "Button action to add a new task.

Argument _BUTTON is for the purpose of using this as a button
action, and is ignored."
  (let ((current (chronometrist-current-task)))
    (when current
      (chronometrist-run-functions-and-clock-out current))
    (let ((task (read-from-minibuffer "New task name: " nil nil nil nil nil t)))
      (chronometrist-run-functions-and-clock-in task))))

;; ## COMMANDS ##

;; TODO - if clocked in and point not on a task, just clock out
(defun chronometrist-toggle-task (&optional prefix inhibit-hooks)
  "Start or stop the task at point.

If there is no task at point, do nothing.

With numeric prefix argument PREFIX, toggle the Nth task in
the buffer. If there is no corresponding task, do nothing.

If INHIBIT-HOOKS is non-nil, the hooks
`chronometrist-before-in-functions',
`chronometrist-after-in-functions',
`chronometrist-before-out-functions', and
`chronometrist-after-out-functions' will not be run."
  (interactive "P")
  (let* ((empty-file   (chronometrist-common-file-empty-p chronometrist-file))
         (nth          (when prefix (chronometrist-goto-nth-task prefix)))
         (at-point     (chronometrist-task-at-point))
         (target       (or nth at-point))
         (current      (chronometrist-current-task))
         (in-function  (if inhibit-hooks
                           #'chronometrist-in
                         #'chronometrist-run-functions-and-clock-in))
         (out-function (if inhibit-hooks
                           #'chronometrist-out
                         #'chronometrist-run-functions-and-clock-out)))
    ;; do not run hooks - chronometrist-add-new-task will do it
    (cond (empty-file (chronometrist-add-new-task))
          ;; What should we do if the user provides an invalid
          ;; argument? Currently - nothing.
          ((and prefix (not nth)))
          (target ;; do nothing if there's no task at point
           ;; clocked in + target is current = clock out
           ;; clocked in + target is some other task = clock out, clock in to task
           ;; clocked out = clock in
           (when current
             (funcall out-function current))
           (unless (equal target current)
             (funcall in-function target))))))

(defun chronometrist-toggle-task-no-hooks (&optional prefix)
  "Like `chronometrist-toggle-task', but don't run hooks.

With numeric prefix argument PREFIX, toggle the Nth task. If there
is no corresponding task, do nothing."
  (interactive "P")
  (chronometrist-toggle-task prefix t))

(defun chronometrist-add-new-task ()
  "Add a new task."
  (interactive)
  (chronometrist-add-new-task-button nil))

;;;###autoload
(defun chronometrist (&optional arg)
  "Display the user's tasks and the time spent on them today.

Based on their timelog file `chronometrist-file'. This is the
'listing command' for `chronometrist-mode'.

If numeric argument ARG is 1, run `chronometrist-report'.
If numeric argument ARG is 2, run `chronometrist-statistics'."
  (interactive "P")
  (chronometrist-migrate-check)
  (let ((buffer (get-buffer-create chronometrist-buffer-name))
        (w      (save-excursion
                  (get-buffer-window chronometrist-buffer-name t))))
    (cond
     (arg (cl-case arg
            (1 (chronometrist-report))
            (2 (chronometrist-statistics))))
     (w (with-current-buffer buffer
          (setq chronometrist--point (point))
          (kill-buffer chronometrist-buffer-name)))
     (t (with-current-buffer buffer
          (cond ((or (not (file-exists-p chronometrist-file))
                     (chronometrist-common-file-empty-p chronometrist-file))
                 ;; first run
                 (chronometrist-common-create-file)
                 (let ((inhibit-read-only t))
                   (chronometrist-common-clear-buffer buffer)
                   (insert "Welcome to Chronometrist! Hit RET to ")
                   (insert-text-button "start a new task."
                                       'action #'chronometrist-add-new-task-button
                                       'follow-link t)
                   (chronometrist-mode)
                   (switch-to-buffer buffer)))
                (t (chronometrist-mode)
                   (when chronometrist-hide-cursor
                     (make-local-variable 'cursor-type)
                     (setq cursor-type nil)
                     (hl-line-mode))
                   (switch-to-buffer buffer)
                   (if (hash-table-keys chronometrist-events)
                       (chronometrist-refresh)
                     (chronometrist-refresh-file nil))
                   (if chronometrist--point
                       (goto-char chronometrist--point)
                     (chronometrist-goto-last-task))))
          (unless chronometrist--fs-watch
            (setq chronometrist--fs-watch
                  (file-notify-add-watch chronometrist-file '(change) #'chronometrist-refresh-file))))))))

(provide 'chronometrist)

;;; chronometrist.el ends here
