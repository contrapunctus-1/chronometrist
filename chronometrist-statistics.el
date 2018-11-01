(require 'chronometrist-lib)
(require 'chronometrist-events)
(require 'chronometrist-statistics-custom)

;; for each activity -
;; + days on which time spent - int (float percent)
;;   - get days in range - count days on which worked on activity
;; + average time spent - HH:MM:SS
;;   - get days in range - get time spent per day for activity in range - get the length and the total -> (/ (+ durations) length)
;; + current streak - [int years, int months,] int days
;;   - get days in range - calculate all streaks - see if last streak is on current day
;; + last streak    - [int years, int months,] int days
;;   - get days in range - calculate all streaks - if current streak, return second-last streak, else return last streak
;; + longest streak - [int years, int months,] int days
;;   - get days in range - calculate all streaks - find longest streak
;; ...where "range" is a week, a month, a year, the entire range of records, or an arbitrary date range

;; details!
;; for each activity, spent most time on doing X (where X is a
;; comment, assuming you use comments to detail what you did)

;; Really might need emacs-async for this...buttloads of big
;; calculations which will only get bigger as the timelog file grows,
;; and the more the activities the more the calculations! I'm
;; visualizing the table loading gradually, field by field, like an
;; image in a browser.

;; unused function
(defun chronometrist-date->time (date)
  "Converts DATE to a time value (see (info \"(elisp)Time of Day\")).
DATE must be a list in the form (YEAR MONTH DAY)."
  (->> date (reverse) (apply #'encode-time 0 0 0)))

;; unused function
(defun chronometrist-date-less-p (date1 date2)
  "Like `time-less-p' but for dates. Returns ‘t’ if date1 is less
than date2. Both must be lists in the form (YEAR MONTH DAY)."
  (time-less-p (chronometrist-date->time date1) (chronometrist-date->time date2)))

(defun chronometrist-statistics-count-active-days (project &optional table)
  "Return the number of days the user spent a non-zero amount of
time on PROJECT, based on their `timeclock-file'. TABLE must be a
hash table - if not supplied, `chronometrist-events' is used.

This will not return correct results if TABLE contains records
which span midnights. (see `chronometrist-clean-ht')"
  (let ((count 0)
        (table (if table table chronometrist-events)))
    (maphash (lambda (date events)
               (when (seq-find (lambda (event)
                                 (and (equal (elt event 0) "i")
                                      (equal (elt event 7) project)))
                               events)
                 (setq count (1+ count))))
             table)
    count))

(defun chronometrist-statistics-count-average-time-spent (project &optional table)
  "Return the average time the user has spent on PROJECT based on
their `timeclock-file'. TABLE must be a hash table - if not
supplied, `chronometrist-events' is used.

This will not return correct results if TABLE contains records
which span midnights. (see `chronometrist-clean-ht')")

;; ## TIMER ##

(defun chronometrist-statistics-timer ()
  (when (get-buffer-window chronometrist-statistics-buffer-name t)
    (with-current-buffer chronometrist-statistics-buffer-name
      (chronometrist-statistics-refresh))))

(defun chronometrist-statistics-maybe-start-timer ()
  (unless chronometrist-statistics--timer-object
    (setq chronometrist-statistics--timer-object
          (run-at-time t chronometrist-statistics-update-interval #'chronometrist-statistics-timer))
    t))

(defvar chronometrist-statistics--timer-object nil)

(defun chronometrist-change-update-interval (arg)
  (interactive "NEnter new interval (in seconds): ")
  (cancel-timer chronometrist-statistics--timer-object)
  (setq chronometrist-statistics-update-interval arg
        chronometrist-statistics--timer-object nil)
  (chronometrist-statistics-maybe-start-timer))

;; ## VARIABLES ##

(defvar chronometrist-statistics--ui-state nil
  "The display state for `chronometrist-statistics'. Must be a
plist in the form (:mode :start-date :end-date).

:MODE is either 'week, 'month, 'year, 'full, or 'custom.

'week, 'month, and 'year mean display statistics
weekly/monthly/yearly respectively.

'full means display statistics from the beginning to the end of
the `timeclock-file'.

'custom means display statistics from an arbitrary date range.

:START-DATE and :END-DATE are the start and end of the date range
to be displayed. They must be dates in the form (YEAR MONTH DAY).")

(defvar chronometrist-statistics--point nil)

;; ## FUNCTIONS ##

(defun chronometrist-statistics-entries ()
  "Creates entries to be displayed in the buffer created by
`chronometrist-statistics', as specified by `tabulated-list-entries'."
  (mapcar (lambda (project)
            (list project
                  (vector project
                          (format "% 10s"
                                  (chronometrist-statistics-count-project-days project)))))
          timeclock-project-list))

(defun chronometrist-statistics-format-keybinds (command &optional firstonly)
  (if firstonly
      (key-description
       (where-is-internal command chronometrist-statistics-mode-map firstonly))
      (->> (where-is-internal command chronometrist-statistics-mode-map)
           (mapcar #'key-description)
           (-take 2)
           (-interpose ", ")
           (apply #'concat))))

(defun chronometrist-statistics-print-keybind (command &optional description firstonly)
  (insert "\n    "
          (chronometrist-statistics-format-keybinds command firstonly)
          " - "
          (if description description "")))

(defun chronometrist-statistics-refresh ()
  (with-current-buffer chronometrist-statistics-buffer-name
    (let* ((w  (get-buffer-window chronometrist-statistics-buffer-name t))
           (wp (window-point w))
           (p  (point)))
      (timeclock-reread-log)
      (tabulated-list-print t nil)
      (chronometrist-statistics-print-non-tabular)
      (chronometrist-statistics-maybe-start-timer)
      (if (equal w (frame-selected-window))
          (goto-char (or chronometrist-statistics--point p))
        (set-window-point w wp)))))

;; ## MAJOR MODE ##

(defvar chronometrist-statistics-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") #'chronometrist-open-timeclock-file)
    (define-key map (kbd "b") #'chronometrist-statistics-previous-range)
    (define-key map (kbd "f") #'chronometrist-statistics-next-range)
    map)
  "Keymap used by `chronometrist-statistics-mode'.")

(define-derived-mode chronometrist-statistics-mode tabulated-list-mode "Chronometrist-Statistics"
  "Major mode for `chronometrist-statistics'."
  (timeclock-reread-log)

  (make-local-variable 'tabulated-list-format)
  (setq tabulated-list-format
        [("Project"     25 t)
         ("Active days" 10 t)
         ;; ("Average time spent"       10 t)
         ;; ("Current streak"           10 t)
         ;; ("Last streak"              10 t)
         ;; ("Longest streak"           10 t)
         ])

  (make-local-variable 'tabulated-list-entries)
  (setq tabulated-list-entries 'chronometrist-statistics-entries)

  (make-local-variable 'tabulated-list-sort-key)
  (setq tabulated-list-sort-key '("Project" . nil))

  (tabulated-list-init-header)

  ;; (chronometrist-statistics-maybe-start-timer)
  )

;; ## COMMANDS ##

(defun chronometrist-statistics (&optional preserve-state)
  "Display statistics of the user's timeclock.el projects, based
on their timelog file in `timeclock-file'. This is the 'listing
command' for chronometrist-statistics-mode.

If a buffer called `chronometrist-statistics-buffer-name' already
exists and is visible, kill the buffer.

If PRESERVE-STATE is nil (the default when not supplied), set
`chronometrist-statistics--ui-state' to nil and display data from
the current week. Otherwise, display data from the week specified
by `chronometrist-statistics--ui-state'."
  (interactive)
  (let ((buffer (get-buffer-create chronometrist-statistics-buffer-name)))
    (with-current-buffer buffer
      (cond ((get-buffer-window chronometrist-statistics-buffer-name)
             (kill-buffer buffer))
            (t ;; (delete-other-windows)
             (when (not preserve-state)
               (setq chronometrist-statistics--ui-state nil))
             (chronometrist-common-create-timeclock-file)
             (chronometrist-statistics-mode)
             (switch-to-buffer buffer)
             ;; (chronometrist-statistics-refresh)
             (tabulated-list-print))))))

(defun chronometrist-statistics-previous-range (arg)
  "View the statistics in the previous time range."
  (interactive "P")
  (let ((arg (if (and arg (numberp arg))
                 (abs arg)
               1)))
    (if chronometrist-statistics--ui-state
        (setq chronometrist-statistics--ui-state
              (chronometrist-statistics-increment-or-decrement-date chronometrist-statistics--ui-state '- (* 7 arg)))
      (setq chronometrist-statistics--ui-state
            (chronometrist-statistics-increment-or-decrement-date (decode-time) '- (* 7 arg))))
    (setq chronometrist-statistics--point (point))
    (kill-buffer)
    (chronometrist-statistics t)))

(defun chronometrist-statistics-next-range (arg)
  "View the statistics in the next time range."
  (interactive "P")
  (let ((arg (if (and arg (numberp arg))
                 (abs arg)
               1)))
    (if chronometrist-statistics--ui-state
        (setq chronometrist-statistics--ui-state
              (chronometrist-statistics-increment-or-decrement-date chronometrist-statistics--ui-state '+ (* 7 arg)))
      (setq chronometrist-statistics--ui-state
            (chronometrist-statistics-increment-or-decrement-date (decode-time) '+ (* 7 arg))))
    (setq chronometrist-statistics--point (point))
    (kill-buffer)
    (chronometrist-statistics t)))

(provide 'chronometrist-statistics)

;; Local Variables:
;; nameless-current-name: "chronometrist-statistics"
;; End:
