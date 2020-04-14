;;; chronometrist.el --- A time tracker for Emacs with a nice interface -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabber.fr>
;; Maintainer: contrapunctus <xmpp:contrapunctus@jabber.fr>
;; Keywords: calendar
;; Homepage: https://framagit.org/contrapunctus/chronometrist
;; Package-Requires: ((emacs "25.1") (dash "2.16.0") (cl-lib "1.0") (seq "2.20"))
;; Version: 0.4.0

(require 'filenotify)
(require 'cl-lib)
(require 'subr-x)

(require 'chronometrist-common)
(require 'chronometrist-timer)
(require 'chronometrist-custom)
(require 'chronometrist-report)
(require 'chronometrist-statistics)
(require 'chronometrist-sexp)
(require 'chronometrist-queries)
(require 'chronometrist-migrate)

;;; Commentary:
;;

;; modifiers to toggling -
;; Nth task
;; reason (ask on start/ask on end/don't ask on end)
;; run/don't run hooks (maybe there should be a function to toggle this)

;; Style issues
;; 1. Uses Scheme-style ? and x->y naming conventions instead of
;;    Elisp/CL-style "-p" and "x-to-y"
;;    - ido uses ? for 'completion help', so you can't type ? unless
;;      you unset that o\
;; 2. Should use *earmuffs* for global variables for clarity
;; 3. Should names of major modes (chronometrist-mode,
;;    chronometrist-report-mode) end with -major-mode ?

;; Limitations of tabulated-list-mode
;; 1. Can't mix tabulated and non-tabulated data!!! What if I want
;;    some buttons, separate from the data but part of the same
;;    buffer?!
;;    - adding non-tabular data after calling `tabulated-list-print' -
;;      as we do - works, but is hacky and doesn't always print (e.g.
;;      it vanishes when you sort). Then, you have to ensure you call
;;      it after each time you call `tabulated-list-print' :\
;;    - a post-print hook could help
;;    - maybe use advice?
;; 2. Can't have multi-line headers
;; 3. Can't have multiple tables in a buffer

;; ## VARIABLES ##
;;; Code:

(defvar chronometrist--task-history nil)
(defvar chronometrist--point nil)
(defvar chronometrist-mode-map)

;; ## FUNCTIONS ##
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
  (->> chronometrist-task-list
       (-sort #'string-lessp)
       (--map-indexed
        (let* ((task        it)
               (index       (number-to-string (1+ it-index)))
               (task-button (list task
                                  'action 'chronometrist-toggle-task-button
                                  'follow-link t))
               (task-time   (-> (chronometrist-task-time-one-day task)
                                (chronometrist-format-time)))
               (indicator   (if (chronometrist-task-active? task)
                                (chronometrist-activity-indicator)
                              ""))
               (target      (chronometrist-get-target task))
               (target-str  (if target
                                (format "% 4d" target)
                              "")))
          (list task
                (vconcat (vector index task-button task-time indicator)
                         (if chronometrist-time-targets-list
                             (vector target-str)
                           [])))))))

(defun chronometrist-task-at-point ()
  "Return the task at point in the `chronometrist' buffer, or nil if there is no task at point."
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward "[0-9]+ +" nil t)
        (--> (buffer-substring-no-properties
              (point)
              (progn
                (re-search-forward chronometrist-time-re-ui nil t)
                (match-beginning 0)))
             (replace-regexp-in-string "[ \t]*$" "" it))
      nil)))

(defun chronometrist-goto-last-task ()
  "In the `chronometrist' buffer, move point to the line containing the last active task."
  (goto-char (point-min))
  ;; FIXME
  ;; (re-search-forward timeclock-last-project nil t)
  (beginning-of-line))

(defun chronometrist-print-keybind (command &optional description firstonly)
  "Insert the keybindings for COMMAND.
If DESCRIPTION is non-nil, insert that too.
If FIRSTONLY is non-nil, return only the first keybinding found."
  (insert
   "\n"
   (format "% 18s - %s"
           (chronometrist-format-keybinds command
                             chronometrist-mode-map
                             firstonly)
           (if description description ""))))

(defun chronometrist-print-non-tabular ()
  "Print the non-tabular part of the buffer in `chronometrist'."
  (with-current-buffer chronometrist-buffer-name
    (let ((inhibit-read-only t)
          (w "\n    ")
          ;; (keybind-start-new (chronometrist-format-keybinds 'chronometrist-add-new-task
          ;;                                      chronometrist-mode-map))
          (keybind-toggle    (chronometrist-format-keybinds 'chronometrist-toggle-task
                                               chronometrist-mode-map
                                               t)))
      (goto-char (point-max))
      (-->
       (chronometrist-active-time-one-day)
       (chronometrist-format-time it)
       (format "%s%- 26s%s" w "Total" it)
       (insert it))

      (insert "\n")
      (insert w (format "% 17s" "Keys")
              w (format "% 17s" "----"))

      (chronometrist-print-keybind 'chronometrist-add-new-task)
      (insert-text-button "start a new task"
                          'action #'chronometrist-add-new-task-button
                          'follow-link t)

      (chronometrist-print-keybind 'chronometrist-toggle-task
                      "toggle task at point")

      (chronometrist-print-keybind 'chronometrist-toggle-task-no-reason
                      "toggle without asking for reason")

      (insert "\n " (format "%s %s - %s"
                            "<numeric argument N>"
                            keybind-toggle
                            "toggle <N>th task"))

      (chronometrist-print-keybind 'chronometrist-report)
      (insert-text-button "see weekly report"
                          'action #'chronometrist-report
                          'follow-link t)

      (chronometrist-print-keybind 'chronometrist-open-file)
      (insert-text-button "open log file"
                          'action #'chronometrist-open-file
                          'follow-link t)
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

(defun chronometrist-refresh-file (_fs-event)
  "Re-read `chronometrist-file' and refresh the `chronometrist' buffer.
Argument _FS-EVENT is ignored."
  ;; (chronometrist-file-clean)
  (chronometrist-events-populate)
  (setq chronometrist-task-list (chronometrist-tasks-from-table))
  (chronometrist-tags-history-populate)
  (chronometrist-key-history-populate)
  (chronometrist-value-history-populate)
  (chronometrist-refresh))

(defun chronometrist-query-stop ()
  "Ask the user if they would like to clock out."
  (interactive)
  (let ((task (chronometrist-current-task)))
    (and task
         (yes-or-no-p (concat "Stop tracking time for " task "? "))
         (chronometrist-out))
    t))

;; ## HOOKS ##

(defvar chronometrist-before-in-functions nil
  "Functions to run before a task is clocked in.
Each function in this hook must accept a single argument, which
is the name of the task to be clocked-in.

The commands `chronometrist-toggle-task-button',
`chronometrist-add-new-task-button',
`chronometrist-toggle-task',
`chronometrist-add-new-task', and
`chronometrist-toggle-task-no-reason' will run this hook.")

(defvar chronometrist-after-in-functions nil
  "Functions to run after a task is clocked in.
Each function in this hook must accept a single argument, which
is the name of the task to be clocked-in.

The commands `chronometrist-toggle-task-button',
`chronometrist-add-new-task-button',
`chronometrist-toggle-task',
`chronometrist-add-new-task', and
`chronometrist-toggle-task-no-reason' will run this hook.")

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
    (define-key map (kbd "M-RET") #'chronometrist-toggle-task-no-reason)
    (define-key map (kbd "l")     #'chronometrist-open-file)
    (define-key map (kbd "r")     #'chronometrist-report)
    (define-key map [mouse-1]     #'chronometrist-toggle-task)
    (define-key map [mouse-3]     #'chronometrist-toggle-task-no-reason)
    (define-key map (kbd "a")     #'chronometrist-add-new-task)
    map)
  "Keymap used by `chronometrist-mode'.")

(define-derived-mode chronometrist-mode tabulated-list-mode "Chronometrist"
  "Major mode for `chronometrist'."
  (make-local-variable 'tabulated-list-format)
  (setq tabulated-list-format
        (vconcat [("#"       3  t)
                  ("Task"    25 t)
                  ("Time"    10 t)
                  ("Active"  10 t)]
                 (when chronometrist-time-targets-list
                   [("Target" 3 t)])))
  (make-local-variable 'tabulated-list-entries)
  (setq tabulated-list-entries 'chronometrist-entries)
  (make-local-variable 'tabulated-list-sort-key)
  (setq tabulated-list-sort-key '("Task" . nil))
  (tabulated-list-init-header)
  (setq revert-buffer-function #'chronometrist-refresh))

;; ## BUTTONS ##

;; FIXME - there is duplication between this function and `chronometrist-toggle-task's logic
(defun chronometrist-toggle-task-button (_button)
  "Button action to toggle a task.

Argument _BUTTON is for the purpose of using this as a button
action, and is ignored."
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
;; PROFILE
;; TODO - implement `chronometrist-ask-tags-p' and `chronometrist-ask-key-values-p' (don't prompt for them if nil)
(defun chronometrist-toggle-task (&optional prefix)
  "Start or stop the task at point.

If there is no task at point, do nothing.

With numeric prefix argument PREFIX, toggle the Nth task in
the buffer. If there is no corresponding task, do nothing."
  (interactive "P")
  (let* ((empty-file (chronometrist-common-file-empty-p chronometrist-file))
         (nth        (when prefix (chronometrist-goto-nth-task prefix)))
         (at-point   (chronometrist-task-at-point))
         (target     (or nth at-point))
         (current    (chronometrist-current-task)))
    (cond (empty-file (chronometrist-add-new-task)) ;; do not run hooks - chronometrist-add-new-task will do it
          ;; What should we do if the user provides an invalid argument? Currently - nothing.
          ((and prefix (not nth)))
          (target ;; do nothing if there's no task at point
           ;; clocked in + target is current = clock out
           ;; clocked in + target is some other task = clock out, clock in to task
           ;; clocked out = clock in
           (when current
             (chronometrist-run-functions-and-clock-out current))
           (unless (equal target current)
             (chronometrist-run-functions-and-clock-in target))))))

(defun chronometrist-toggle-task-no-reason (&optional prefix)
  "Like `chronometrist-toggle-task', but don't ask for a reason.

With numeric prefix argument PREFIX, toggle the Nth task. If there
is no corresponding task, do nothing."
  (interactive "P")
  (chronometrist-toggle-task prefix))

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
        (w      (get-buffer-window chronometrist-buffer-name t)))
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
                 (chronometrist-common-create-chronometrist-file)
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
                  (file-notify-add-watch chronometrist-file
                                         '(change)
                                         #'chronometrist-refresh-file))))))))

(provide 'chronometrist)

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:

;;; chronometrist.el ends here
