(require 'chronometrist-common)
(require 'chronometrist-custom)
(require 'chronometrist-report)
(require 'chronometrist-statistics)
(require 'chronometrist-assist)

;; TODO - don't suggest nil when asking for first project on first run

;; TODO - use variables instead of hardcoded numbers to determine spacing

;; TODO - remove repetitive calls to (format "%04d-%02d-%02d" (elt seq a) (elt seq b) (elt seq c))

;; TODO - when starting a project with time of "-" (i.e. not worked on
;; today until now), immediately set time to 0 instead of waiting for
;; the first timer refresh

;; TODO - Mouse commands should work only on buttons.

;; TODO - mouse-3 should clock-out without asking for reason.

;; TODO - some way to ask for the reason just before starting a project
;; Even when clocking out, the reason is asked _before_ clocking out,
;; which adds time to the project.

;; modifiers to toggling -
;; Nth task
;; reason (ask on start/ask on end/don't ask on end)
;; run/don't run hooks (maybe there should be a function to toggle this)

;; TODO - timeclock already _has_ hooks! :| Why do we re-implement them?

;; Style issues
;; 1. Uses Scheme-style ? and x->y naming conventions instead of
;;    Elisp/CL-style "-p" and "x-to-y"
;;    - ido uses ? for 'completion help', so you can't type ? unless
;;      you unset that o\
;; 2. Should use *earmuffs* for global variables for clarity
;; 3. Should names of major modes (chronometrist-mode,
;;    chronometrist-report-mode) end with -major-mode ?

;; Limitations of timeclock.el
;; 1. Concurrent tasks not permitted
;; 2. timeclock-project-list contains only the projects found in the
;;    timeclock-file - no way for a user to specify tasks beforehand.
;; 3. Uses non-standard slashes in the date instead of dashes (e.g.
;;    "2018/01/01" instead of "2018-01-01") and a space for the
;;    date-time separator instead of T

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
(defvar chronometrist--timer-object nil)

(defvar chronometrist--point nil)

(add-hook 'first-change-hook 'chronometrist-assist)

;; ## TIMER ##
(defun chronometrist-timer ()
  (when (get-buffer chronometrist-buffer-name)
    (chronometrist-refresh)))

(defun chronometrist-stop-timer ()
  (interactive)
  (cancel-timer chronometrist--timer-object)
  (setq chronometrist--timer-object nil))

(defun chronometrist-maybe-start-timer ()
  "If `chronometrist--timer-object' is non-nil, add
`chronometrist-timer' to the list of active timers and return t,
else do nothing and return nil."
  (interactive)
  (unless chronometrist--timer-object
    (setq chronometrist--timer-object
          (run-at-time t chronometrist-update-interval #'chronometrist-timer))
    t))

(defun chronometrist-change-update-interval (arg)
  (interactive "NEnter new interval (in seconds): ")
  (cancel-timer chronometrist--timer-object)
  (setq chronometrist-update-interval arg
        chronometrist--timer-object nil)
  (chronometrist-maybe-start-timer))

;; ## FUNCTIONS ##
(defun chronometrist-current-project ()
  "Return the name of the currently clocked-in project, or nil if
 the user is not clocked in."
  (if (not (timeclock-currently-in-p))
      nil
    (with-current-buffer (find-file-noselect timeclock-file)
      (save-excursion
        (goto-char (point-max))
        (forward-line -1)
        (re-search-forward (concat chronometrist-time-re-file " ") nil t)
        (buffer-substring-no-properties (point) (point-at-eol))))))

(defun chronometrist-project-active? (project)
  "Return t if PROJECT is currently clocked in, else nil."
  (equal (chronometrist-current-project) project))

(defun chronometrist-seconds-to-hms (seconds)
  "Convert SECONDS to a vector in the form [HOURS MINUTES
SECONDS]. SECONDS must be a positive integer."
  (setq seconds (truncate seconds))
  (let* ((s (% seconds 60))
         (m (% (/ seconds 60) 60))
         (h (/ seconds 3600)))
    (vector h m s)))

(defun chronometrist-entries ()
  "Create entries to be displayed in the buffer created by
`chronometrist'."
  (timeclock-reread-log)
  (chronometrist-events-populate)
  (chronometrist-events-clean)
  (->> timeclock-project-list
       (-sort #'string-lessp)
       (--map-indexed
        (list it
              (vector (number-to-string (1+ it-index))
                      (list it
                            'action 'chronometrist-toggle-project-button
                            'follow-link t)
                      (-> (chronometrist-project-time-one-day it)
                          (chronometrist-format-time))
                      (if (chronometrist-project-active? it)
                          "*" ""))))))

(defun chronometrist-project-at-point ()
  "Return the project at point in the `chronometrist' buffer, or
nil if there is no project at point."
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
  (goto-char (point-min))
  (re-search-forward timeclock-last-project nil t)
  (beginning-of-line))

(defun chronometrist-time-add (a b)
  "Add two vectors in the form [HOURS MINUTES SECONDS] and
return a vector in the same form."
  (let ((h1 (elt a 0))
        (m1 (elt a 1))
        (s1 (elt a 2))
        (h2 (elt b 0))
        (m2 (elt b 1))
        (s2 (elt b 2)))
    (chronometrist-seconds-to-hms (+ (* h1 3600) (* h2 3600)
                        (* m1 60) (* m2 60)
                        s1 s2))))

(defun chronometrist-total-time-one-day (&optional date)
  "Return the total time clocked on DATE (if non-nil) or
 today, as a vector in the form [HOURS MINUTES SECONDS].

DATE must be calendrical information calendrical
information (see (info \"(elisp)Time Conversion\"))."
  (->> timeclock-project-list
       (--map (chronometrist-project-time-one-day it date))
       (-reduce #'chronometrist-time-add)))

(defun chronometrist-format-keybinds (command &optional firstonly)
  (if firstonly
      (key-description
       (where-is-internal command chronometrist-mode-map firstonly))
      (->> (where-is-internal command chronometrist-mode-map)
           (mapcar #'key-description)
           (-take 2)
           (-interpose ", ")
           (apply #'concat))))

(defun chronometrist-print-keybind (command &optional description firstonly)
  (insert
   "\n"
   (format "% 18s - %s"
           (chronometrist-format-keybinds command firstonly)
           (if description description ""))))

(defun chronometrist-print-non-tabular ()
  "Print the non-tabular part of the buffer in `chronometrist'."
  (with-current-buffer chronometrist-buffer-name
    (let ((inhibit-read-only t)
          (w "\n    ")
          (keybind-start-new (chronometrist-format-keybinds 'chronometrist-add-new-project))
          (keybind-toggle    (chronometrist-format-keybinds 'chronometrist-toggle-project t)))
      (goto-char (point-max))
      (-->
       (chronometrist-total-time-one-day)
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

      (chronometrist-print-keybind 'chronometrist-open-timeclock-file)
      (insert-text-button "open log file"
                          'action #'chronometrist-open-timeclock-file
                          'follow-link t)
      (insert "\n"))))

(defun chronometrist-goto-nth-project (n)
  "Move point to the beginning of the line containing the Nth
project in a `chronometrist' buffer. Return the project at point,
or nil if there is no corresponding project. N must be a positive
integer."
  (goto-char (point-min))
  (when (re-search-forward (format "^%d" n) nil t)
    (beginning-of-line)
    (chronometrist-project-at-point)))

(defun chronometrist-refresh (&optional ignore-auto noconfirm)
  (let* ((w (get-buffer-window chronometrist-buffer-name t))
         (p (window-point w)))
    (with-current-buffer chronometrist-buffer-name
      (chronometrist-events-populate)
      (chronometrist-events-clean)
      (timeclock-reread-log)
      (tabulated-list-print t nil)
      (chronometrist-print-non-tabular)
      (chronometrist-maybe-start-timer)
      (set-window-point w p))))

;; FIXME - has some duplicate logic with `chronometrist-project-events-in-day'
(defun chronometrist-reason-list (project)
  "Filters `timeclock-reason-list' to only return reasons for PROJECT."
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
  "Replacement for `timeclock-ask-for-reason' which uses
`read-from-minibuffer' instead of `completing-read'. (see
`timeclock-get-reason-function')

Additionally, it uses `chronometrist-reason-list' to only suggest
reasons used for the relevant project, instead of all reasons as
in `timeclock-reason-list'."
  (let ((reason-history (chronometrist-reason-list timeclock-last-project)))
    (read-from-minibuffer "Reason for clocking out (optional): " nil nil nil
                          'reason-history)))

;; ## HOOKS ##

(defvar chronometrist-project-start-hook nil
  "Hook run before a project is clocked in. Each function in this hook must accept a single argument, which is the project to be clocked-in.

The commands `chronometrist-toggle-project-button',
`chronometrist-add-new-project-button',
`chronometrist-toggle-project',
`chronometrist-add-new-project', and
`chronometrist-toggle-project-no-reason' will run this hook.")

(defvar chronometrist-project-stop-hook nil
  "Hook run after a project is clocked out. Each function in this
hook must accept a single argument, which is the clocked-out
project.")

(defun chronometrist-run-project-start-hook (project)
  (run-hook-with-args 'chronometrist-project-start-hook project))

(defun chronometrist-run-project-end-hook (project)
  (run-hook-with-args 'chronometrist-project-stop-hook project))

;; ## MAJOR-MODE ##
(defvar chronometrist-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET")   #'chronometrist-toggle-project)
    (define-key map (kbd "M-RET") #'chronometrist-toggle-project-no-reason)
    (define-key map (kbd "l")     #'chronometrist-open-timeclock-file)
    (define-key map (kbd "r")     #'chronometrist-report)
    (define-key map [mouse-1]     #'chronometrist-toggle-project)
    (define-key map [mouse-3]     #'chronometrist-toggle-project-no-reason)
    (define-key map (kbd "a")     #'chronometrist-add-new-project)
    map)
  "Keymap used by `chronometrist-mode'.")

(define-derived-mode chronometrist-mode tabulated-list-mode "Chronometrist"
  "Major mode for `chronometrist'."
  (timeclock-reread-log)
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
  (setq revert-buffer-function #'chronometrist-refresh
        timeclock-get-reason-function #'chronometrist-ask-for-reason))

;; ## BUTTONS ##

;; FIXME - there is duplication between this function and `chronometrist-toggle-project's logic
(defun chronometrist-toggle-project-button (button)
  (let ((current  (chronometrist-current-project))
        (at-point (chronometrist-project-at-point)))
    ;; clocked in + point on current    = clock out
    ;; clocked in + point on some other project = clock out, clock in to project
    ;; clocked out = clock in
    (when current
      (timeclock-out nil nil t)
      (chronometrist-run-project-end-hook current))
    (unless (equal at-point current)
      (chronometrist-run-project-start-hook at-point)
      (timeclock-in nil at-point nil))
    (chronometrist-refresh)))

(defun chronometrist-add-new-project-button (button)
  (let ((current (chronometrist-current-project)))
    (when current
      (timeclock-out nil nil t)
      (chronometrist-run-project-end-hook current))
    (let ((p (read-from-minibuffer "New project name: " nil nil nil nil nil t)))
      (chronometrist-run-project-start-hook p)
      (timeclock-in nil p nil))
    (chronometrist-refresh)))

;; ## COMMANDS ##

;; TODO - if clocked in and point not on a project, just clock out
(defun chronometrist-toggle-project (&optional prefix no-prompt)
  "In a `chronometrist' buffer, start or stop the project at
point. If there is no project at point, do nothing.

With a numeric prefix argument, toggle the Nth project. If there
is no corresponding project, do nothing."
  (interactive "P")
  (let* ((empty-file (chronometrist-common-file-empty-p timeclock-file))
         (nth        (when prefix (chronometrist-goto-nth-project prefix)))
         (at-point   (chronometrist-project-at-point))
         (target     (or nth at-point))
         (current    (chronometrist-current-project))
         (ask        (not no-prompt)))
    (cond (empty-file (chronometrist-add-new-project)) ;; do not run hooks - chronometrist-add-new-project will do it
          ;; What should we do if the user provides an invalid argument? Currently - nothing.
          ((and prefix (not nth)))
          (target ;; do nothing if there's no project at point
           ;; clocked in + target is current = clock out
           ;; clocked in + target is some other project = clock out, clock in to project
           ;; clocked out = clock in
           (when current
             (timeclock-out nil nil ask)
             (chronometrist-run-project-end-hook current))
           (unless (equal target current)
             (chronometrist-run-project-start-hook target)
             (timeclock-in nil target nil))))
    (chronometrist-refresh)))

(defun chronometrist-toggle-project-no-reason (&optional prefix)
  "Like `chronometrist-toggle-project', but do not ask for a
reason if clocking out."
  (interactive "P")
  (funcall-interactively #'chronometrist-toggle-project prefix t))

(defun chronometrist-add-new-project ()
  (interactive)
  (chronometrist-add-new-project-button nil))

(defun chronometrist (&optional arg)
  "Displays a list of the user's timeclock.el projects and the
time spent on each today, based on their timelog file
`timeclock-file'. The user can hit RET to start/stop projects.
This is the 'listing command' for chronometrist-mode.

With numeric argument 1, run `chronometrist-report'.
With numeric argument 2, run `chronometrist-statistics'."
  (interactive "P")
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
          (cond ((or (not (file-exists-p timeclock-file))
                     (chronometrist-common-file-empty-p timeclock-file))
                 ;; first run
                 (chronometrist-common-create-timeclock-file)
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
                   (chronometrist-refresh)
                   (if chronometrist--point
                       (goto-char chronometrist--point)
                     (chronometrist-goto-last-project)))))))))

(provide 'chronometrist)

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:
