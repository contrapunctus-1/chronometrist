;;; chronometrist.el --- A time tracker for Emacs with a nice interface -*- lexical-binding: t; -*-

(require 'filenotify)
(require 'chronometrist-common)
(require 'chronometrist-timer)
(require 'chronometrist-custom)
(require 'chronometrist-history)
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

(defvar chronometrist--timer-object nil)
(defvar chronometrist--project-history nil)
(defvar chronometrist--point nil)
(defvar chronometrist-task-list nil)

;; ## FUNCTIONS ##
(defun chronometrist-current-task ()
  "Return the name of the currently clocked-in task, or nil if not clocked in."
  (let ((last-event (chronometrist-last-expr)))
    (if (plist-member last-event :stop)
        nil
      (plist-get last-event :name))))

(defun chronometrist-task-active? (task)
  "Return t if TASK is currently clocked in, else nil."
  (equal (chronometrist-current-task) task))

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
        (list it
              (vector (number-to-string (1+ it-index))
                      (list it
                            'action 'chronometrist-toggle-project-button
                            'follow-link t)
                      (-> (chronometrist-task-time-one-day it)
                          (chronometrist-format-time))
                      (if (chronometrist-task-active? it)
                          "*" ""))))))

(defun chronometrist-project-at-point ()
  "Return the project at point in the `chronometrist' buffer, or nil if there is no project at point."
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

(defun chronometrist-goto-last-project ()
  "In the `chronometrist' buffer, move point to the line containing the last active project."
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
          (keybind-start-new (chronometrist-format-keybinds 'chronometrist-add-new-project
                                               chronometrist-mode-map))
          (keybind-toggle    (chronometrist-format-keybinds 'chronometrist-toggle-project
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

      (chronometrist-print-keybind 'chronometrist-add-new-project)
      (insert-text-button "start a new project"
                          'action #'chronometrist-add-new-project-button
                          'follow-link t)

      (chronometrist-print-keybind 'chronometrist-toggle-project
                      "toggle project at point")

      (chronometrist-print-keybind 'chronometrist-toggle-project-no-reason
                      "toggle without asking for reason")

      (insert "\n " (format "%s %s - %s"
                            "<numeric argument N>"
                            keybind-toggle
                            "toggle <N>th project"))

      (chronometrist-print-keybind 'chronometrist-report)
      (insert-text-button "see weekly report"
                          'action #'chronometrist-report
                          'follow-link t)

      (chronometrist-print-keybind 'chronometrist-open-file)
      (insert-text-button "open log file"
                          'action #'chronometrist-open-file
                          'follow-link t)
      (insert "\n"))))

(defun chronometrist-goto-nth-project (n)
  "Move point to the line containing the Nth project.
Return the project at point, or nil if there is no corresponding
project. N must be a positive integer."
  (goto-char (point-min))
  (when (re-search-forward (format "^%d" n) nil t)
    (beginning-of-line)
    (chronometrist-project-at-point)))

(defun chronometrist-refresh (&optional ignore-auto noconfirm)
  "Refresh the `chronometrist' buffer, without re-reading `chronometrist-file'.

The optional arguments IGNORE-AUTO and NOCONFIRM are ignored, and
are present solely for the sake of using this function as a value
of `revert-buffer-function'."
  (let* ((w (get-buffer-window chronometrist-buffer-name t))
         (task (window-point w)))
    (when w
      (setq chronometrist-task-list (chronometrist-tasks-from-table))
      (with-current-buffer chronometrist-buffer-name
        (tabulated-list-print t nil)
        (chronometrist-print-non-tabular)
        (chronometrist-maybe-start-timer)
        (set-window-point w task)))))

(defun chronometrist-refresh-file (fs-event)
  "Re-read `chronometrist-file' and refresh the `chronometrist' buffer.
Argument FS-EVENT is ignored."
  ;; (chronometrist-file-clean)
  (chronometrist-events-populate)
  (setq chronometrist-task-list (chronometrist-tasks-from-table))
  (chronometrist-tags-history-populate)
  (chronometrist-key-history-populate)
  (chronometrist-value-history-populate)
  (chronometrist-refresh))

;; HACK - has some duplicate logic with `chronometrist-task-events-in-day'
(defun chronometrist-reason-list (project)
  "Filters `timeclock-reason-list' to only return reasons for PROJECT."
  (declare (obsolete nil "Chronometrist v0.3.0"))
  (let (save-next results)
    (maphash (lambda (date events)
               (seq-do (lambda (event)
                         (cond ((and (equal "i"     (chronometrist-vfirst event))
                                     (equal project (chronometrist-vlast event)))
                                (setq save-next t))
                               (save-next
                                (->> (chronometrist-vlast event)
                                     (list)
                                     (append results)
                                     (setq results))
                                (setq save-next nil))
                               (t nil)))
                       events))
             chronometrist-events)
    (->> results
         ;; the order of reverse and seq-uniq is important, so that
         ;; the most recent reasons come first in the history
         (reverse)
         (seq-uniq)
         (seq-remove (lambda (elt)
                       (equal elt ""))))))

(defun chronometrist-ask-for-reason ()
  "Replacement for `timeclock-ask-for-reason'.

Uses `read-from-minibuffer' instead of `completing-read'. \(see
`timeclock-get-reason-function')

Additionally, it uses `chronometrist-reason-list' to only suggest
reasons used for the relevant project, instead of all reasons as
in `timeclock-reason-list'."
  (declare (obsolete nil "Chronometrist v0.3.0"))
  (setq chronometrist--project-history (chronometrist-reason-list timeclock-last-project))
  (read-from-minibuffer "Reason for clocking out (optional): " nil nil nil
                        'chronometrist--project-history))

;; ## HOOKS ##

(defvar chronometrist-before-in-functions nil
  "Functions to run before a project is clocked in.
Each function in this hook must accept a single argument, which
is the name of the project to be clocked-in.

The commands `chronometrist-toggle-project-button',
`chronometrist-add-new-project-button',
`chronometrist-toggle-project',
`chronometrist-add-new-project', and
`chronometrist-toggle-project-no-reason' will run this hook.")

(defvar chronometrist-after-in-functions nil
  "Functions to run after a project is clocked in.
Each function in this hook must accept a single argument, which
is the name of the project to be clocked-in.

The commands `chronometrist-toggle-project-button',
`chronometrist-add-new-project-button',
`chronometrist-toggle-project',
`chronometrist-add-new-project', and
`chronometrist-toggle-project-no-reason' will run this hook.")

(defvar chronometrist-before-out-functions nil
  "Functions to run before a project is clocked out.
Each function in this hook must accept a single argument, which
is the name of the project to be clocked out of.

The project will be stopped only if all functions in this list
return a non-nil value.")

(defvar chronometrist-after-out-functions nil
  "Functions to run after a project is clocked out.
Each function in this hook must accept a single argument, which
is the name of the project to be clocked out of.")

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
    (define-key map (kbd "RET")   #'chronometrist-toggle-project)
    (define-key map (kbd "M-RET") #'chronometrist-toggle-project-no-reason)
    (define-key map (kbd "l")     #'chronometrist-open-file)
    (define-key map (kbd "r")     #'chronometrist-report)
    (define-key map [mouse-1]     #'chronometrist-toggle-project)
    (define-key map [mouse-3]     #'chronometrist-toggle-project-no-reason)
    (define-key map (kbd "a")     #'chronometrist-add-new-project)
    map)
  "Keymap used by `chronometrist-mode'.")

(define-derived-mode chronometrist-mode tabulated-list-mode "Chronometrist"
  "Major mode for `chronometrist'."
  (make-local-variable 'tabulated-list-format)
  (setq tabulated-list-format [("#"       3  t)
                               ("Project" 25 t)
                               ("Time"    10 t)
                               ("Active"  3  t)])
  (make-local-variable 'tabulated-list-entries)
  (setq tabulated-list-entries 'chronometrist-entries)
  (make-local-variable 'tabulated-list-sort-key)
  (setq tabulated-list-sort-key '("Project" . nil))
  (tabulated-list-init-header)
  (setq revert-buffer-function #'chronometrist-refresh))

;; ## BUTTONS ##

;; FIXME - there is duplication between this function and `chronometrist-toggle-project's logic
(defun chronometrist-toggle-project-button (button)
  "Button action to toggle a project.

Argument BUTTON is for the purpose of using this as a button
action, and is ignored."
  (let ((current  (chronometrist-current-task))
        (at-point (chronometrist-project-at-point)))
    ;; clocked in + point on current    = clock out
    ;; clocked in + point on some other project = clock out, clock in to project
    ;; clocked out = clock in
    (when current
      (chronometrist-run-functions-and-clock-out current))
    (unless (equal at-point current)
      (chronometrist-run-functions-and-clock-in at-point))
    (chronometrist-refresh)))

(defun chronometrist-add-new-project-button (button)
  "Button action to add a new project.

Argument BUTTON is for the purpose of using this as a button
action, and is ignored."
  (let ((current (chronometrist-current-task)))
    (when current
      (chronometrist-run-functions-and-clock-out current))
    (let ((task (read-from-minibuffer "New task name: " nil nil nil nil nil t)))
      (chronometrist-run-functions-and-clock-in task))
    (chronometrist-refresh)))

;; ## COMMANDS ##

;; TODO - if clocked in and point not on a project, just clock out
;; PROFILE
;; TODO - implement `chronometrist-ask-tags-p' and `chronometrist-ask-key-values-p' (don't prompt for them if nil)
(defun chronometrist-toggle-project (&optional prefix)
  "Start or stop the project at point.

If there is no project at point, do nothing.

With numeric prefix argument PREFIX, toggle the Nth project in
the buffer. If there is no corresponding project, do nothing."
  (interactive "P")
  (let* ((empty-file (chronometrist-common-file-empty-p chronometrist-file))
         (nth        (when prefix (chronometrist-goto-nth-project prefix)))
         (at-point   (chronometrist-project-at-point))
         (target     (or nth at-point))
         (current    (chronometrist-current-task)))
    (cond (empty-file (chronometrist-add-new-project)) ;; do not run hooks - chronometrist-add-new-project will do it
          ;; What should we do if the user provides an invalid argument? Currently - nothing.
          ((and prefix (not nth)))
          (target ;; do nothing if there's no project at point
           ;; clocked in + target is current = clock out
           ;; clocked in + target is some other project = clock out, clock in to project
           ;; clocked out = clock in
           (when current
             (chronometrist-run-functions-and-clock-out current))
           (unless (equal target current)
             (chronometrist-run-functions-and-clock-in target))))
    (chronometrist-refresh)))

(defun chronometrist-toggle-project-no-reason (&optional prefix)
  "Like `chronometrist-toggle-project', but don't ask for a reason.

With numeric prefix argument PREFIX, toggle the Nth project. If there
is no corresponding project, do nothing."
  (interactive "P")
  (chronometrist-toggle-project prefix t))

(defun chronometrist-add-new-project ()
  "Add a new project."
  (interactive)
  (chronometrist-add-new-project-button nil))

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
     (arg (case arg
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
                   (insert-text-button "start a new project."
                                       'action #'chronometrist-add-new-project-button
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
                     (chronometrist-goto-last-project))))
          (unless chronometrist--fs-watch
            (setq chronometrist--fs-watch
                  (file-notify-add-watch chronometrist-file
                                         '(change)
                                         #'chronometrist-refresh-file))))))))

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:

(provide 'chronometrist)

;;; chronometrist.el ends here
