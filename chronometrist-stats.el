(require 'chronometrist-lib)

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

(defvar chronometrist-events (make-hash-table :test #'equal))

(defun chronometrist-clean-ht ()
  "Clean `chronometrist-events' by splitting intervals which span
midnights into two. For event data to be processed accurately,
this must be called after `chronometrist-populate-ht'."
  ;; if the first event of a day has a code of "o", it's a midnight spanning event
  )

;; TODO - Maybe strip dates from values, since they're part of the key
;; anyway. Consider using a state machine.
(defun chronometrist-populate-ht ()
  "Clears hash table `chronometrist-events' and populates it
using data from `timeclock-file', with each key being a date in
the form (YEAR MONTH DAY). Values are vectors containing events,
where each event is a vector in the form \[CODE YEAR MONTH DAY
HOURS MINUTES SECONDS \"PROJECT-NAME-OR-COMMENT\"].

This function always returns nil."
  (clrhash chronometrist-events)
  (with-current-buffer (find-file-noselect timeclock-file)
    (save-excursion
      (goto-char (point-min))
      (let ((events))
        (while (not (= (point) (point-max)))
          (let* ((event-string       (buffer-substring-no-properties (point-at-bol)
                                                                     (point-at-eol)))
                 (info-re            (concat ". " chronometrist-date-re " " chronometrist-time-re-file))
                 (project-or-comment (->> event-string
                                          (replace-regexp-in-string (concat info-re " ?") "")
                                          (vector)))
                 (the-rest           (--> (concat "\\(" info-re "\\)" ".*")
                                          (replace-regexp-in-string it "\\1" event-string)
                                          (split-string it "[ /:]")
                                          (append (list (car it))
                                                  (mapcar #'string-to-number (-slice it 1 7)))))
                 (key                (-slice the-rest 1 4))
                 (old-value          (gethash key chronometrist-events))
                 (new-value          (vector (vconcat the-rest ;; vconcat converts lists to vectors
                                                      project-or-comment))))
            (if old-value
                (puthash key (vconcat old-value new-value) chronometrist-events)
              (puthash key new-value chronometrist-events)))
          (forward-line))
        nil))))

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

(defun chronometrist-count-project-days (project &optional table)
  "Return the number of days the user worked on PROJECT based on
their `timeclock-file'. TABLE must be a hash table - if not
supplied, `chronometrist-events' is used.

This will not return correct results if TABLE contains records
which span midnights, so make sure the hash table does not have
them."
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

;; ## TIMER ##

(defun chronometrist-stats-timer ()
  (when (get-buffer-window chronometrist-stats-buffer-name t)
    (with-current-buffer chronometrist-stats-buffer-name
      (chronometrist-stats-refresh))))

(defun chronometrist-stats-maybe-start-timer ()
  (unless chronometrist-stats--timer-object
    (setq chronometrist-stats--timer-object
          (run-at-time t chronometrist-stats-update-interval #'chronometrist-stats-timer))
    t))

(defvar chronometrist-stats--timer-object nil)

(defun chronometrist-change-update-interval (arg)
  (interactive "NEnter new interval (in seconds): ")
  (cancel-timer chronometrist-stats--timer-object)
  (setq chronometrist-stats--update-interval arg
        chronometrist-stats--timer-object nil)
  (chronometrist-stats-maybe-start-timer))

;; ## VARIABLES ##

(defvar chronometrist-stats--ui-mode nil
  "The display mode for `chronometrist-stats'. Valid values are
'week, 'month, 'year, 'full, or 'custom.

'week, 'month, and 'year mean display statistics
weekly/monthly/yearly respectively.

'full means display statistics from the beginning to the end of
the `timeclock-file'.

'custom means display statistics from an arbitrary date range.")

(defvar chronometrist-stats--ui-date nil
  "The first date of the week displayed by `chronometrist-stats' (specifically `chronometrist-stats-entries').
A value of nil means the current week. Otherwise, it must be a
list in the form (YEAR WEEK), where WEEK is the numeric week of
the year (1-52).")

(defvar chronometrist-stats--ui-week-dates nil
  "List of dates currently displayed by `chronometrist-stats' (specifically `chronometrist-stats-entries').
Each date is a list containing calendrical information (see (info \"(elisp)Time Conversion\"))")

(defvar chronometrist-stats--point nil)

;; ## FUNCTIONS ##

(defun chronometrist-stats-entries ()
  "Creates entries to be displayed in the buffer created by
`chronometrist-stats', as specified by `tabulated-list-entries'."
  (let* ((week-dates        (chronometrist-stats-date->week-dates))
         (week-dates-string (chronometrist-stats-dates-in-week->string week-dates)))
    (setq chronometrist-stats--ui-week-dates week-dates)
    (mapcar (lambda (project)
              (let ((project-daily-time-list
                     (--map (chronometrist-project-time-one-day project it) week-dates)))
                (list project
                      (vconcat
                       (vector project)
                       (->> project-daily-time-list
                            (mapcar #'chronometrist-format-time)
                            (apply #'vector))
                       (->> project-daily-time-list
                            (-reduce #'chronometrist-time-add)
                            (chronometrist-format-time)
                            (vector))))))
            timeclock-project-list)))

(defun chronometrist-stats-format-keybinds (command &optional firstonly)
  (if firstonly
      (key-description
       (where-is-internal command chronometrist-stats-mode-map firstonly))
      (->> (where-is-internal command chronometrist-stats-mode-map)
           (mapcar #'key-description)
           (-take 2)
           (-interpose ", ")
           (apply #'concat))))

(defun chronometrist-stats-print-keybind (command &optional description firstonly)
  (insert "\n    "
          (chronometrist-stats-format-keybinds command firstonly)
          " - "
          (if description description "")))

(defun chronometrist-stats-refresh ()
  (with-current-buffer chronometrist-stats-buffer-name
    (let* ((w  (get-buffer-window chronometrist-stats-buffer-name t))
           (wp (window-point w))
           (p  (point)))
      (timeclock-reread-log)
      (tabulated-list-print t nil)
      (chronometrist-stats-print-non-tabular)
      (chronometrist-stats-maybe-start-timer)
      (if (equal w (frame-selected-window))
          (goto-char (or chronometrist-stats--point p))
        (set-window-point w wp)))))

;; ## MAJOR MODE ##

(defvar chronometrist-stats-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") #'chronometrist-open-timeclock-file)
    (define-key map (kbd "b") #'chronometrist-stats-previous-range)
    (define-key map (kbd "f") #'chronometrist-stats-next-range)
    map)
  "Keymap used by `chronometrist-stats-mode'.")

(define-derived-mode chronometrist-stats-mode tabulated-list-mode "Chronometrist-Stats"
  "Major mode for `chronometrist-stats'."
  (timeclock-reread-log)

  (make-local-variable 'tabulated-list-format)
  (setq tabulated-list-format
        [("Project"                  25 t)
         ("Days on which time spent" 10 t)
         ("Average time spent"       10 t)
         ("Current streak"           10 t)
         ("Last streak"              10 t)
         ("Longest streak"           10 t)])

  (make-local-variable 'tabulated-list-entries)
  (setq tabulated-list-entries 'chronometrist-stats-entries)

  (make-local-variable 'tabulated-list-sort-key)
  (setq tabulated-list-sort-key '("Project" . nil))

  (tabulated-list-init-header)

  (chronometrist-stats-maybe-start-timer))

;; ## COMMANDS ##

(defun chronometrist-stats (&optional keep-date)
  "Display statistics of the user's timeclock.el projects, based
on their timelog file in `timeclock-file'. This is the 'listing
command' for chronometrist-stats-mode.

If a buffer called `chronometrist-stats-buffer-name' already
exists and is visible, kill the buffer.

If KEEP-DATE is nil (the default when not supplied), set
`chronometrist-stats--ui-date' to nil and display data from the
current week. Otherwise, display data from the week specified by
`chronometrist-stats--ui-date'."
  (interactive)
  (let ((buffer (get-buffer-create chronometrist-stats-buffer-name)))
    (with-current-buffer buffer
      (cond ((and (get-buffer-window chronometrist-stats-buffer-name)
                  (not keep-date))
             (setq chronometrist-stats--point (point))
             (kill-buffer buffer))
            (t (delete-other-windows)
               (when (not keep-date)
                 (setq chronometrist-stats--ui-date nil))
               (chronometrist-common-create-timeclock-file)
               (chronometrist-stats-mode)
               (switch-to-buffer buffer)
               (chronometrist-stats-refresh)
               (goto-char (or chronometrist-stats--point 1)))))))

(defun chronometrist-stats-previous-range (arg)
  "View the statistics in the previous time range."
  (interactive "P")
  (let ((arg (if (and arg (numberp arg))
                 (abs arg)
               1)))
    (if chronometrist-stats--ui-date
        (setq chronometrist-stats--ui-date
              (chronometrist-stats-increment-or-decrement-date chronometrist-stats--ui-date '- (* 7 arg)))
      (setq chronometrist-stats--ui-date
            (chronometrist-stats-increment-or-decrement-date (decode-time) '- (* 7 arg))))
    (setq chronometrist-stats--point (point))
    (kill-buffer)
    (chronometrist-stats t)))

(defun chronometrist-stats-next-range (arg)
  "View the statistics in the next time range."
  (interactive "P")
  (let ((arg (if (and arg (numberp arg))
                 (abs arg)
               1)))
    (if chronometrist-stats--ui-date
        (setq chronometrist-stats--ui-date
              (chronometrist-stats-increment-or-decrement-date chronometrist-stats--ui-date '+ (* 7 arg)))
      (setq chronometrist-stats--ui-date
            (chronometrist-stats-increment-or-decrement-date (decode-time) '+ (* 7 arg))))
    (setq chronometrist-stats--point (point))
    (kill-buffer)
    (chronometrist-stats t)))

(provide 'chronometrist-stats)

;; Local Variables:
;; nameless-current-name: "chronometrist-stats"
;; End:
