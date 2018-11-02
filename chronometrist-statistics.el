(require 'chronometrist-common)
(require 'chronometrist-events)
(require 'chronometrist-statistics-custom)

;; for each activity -
;; [x] days active - int (float percent)
;;     - get days in range - count days on which worked on activity
;; [x] average time - HH:MM:SS
;;     - get days in range - get time spent per day for activity in range - get the length and the total -> (/ (+ durations) length)
;; [ ] current streak - [int years, int months,] int days
;;     - get days in range - calculate all streaks - see if last streak is on current day
;; [ ] last streak    - [int years, int months,] int days
;;     - get days in range - calculate all streaks - if current streak, return second-last streak, else return last streak
;; [ ] longest streak - [int years, int months,] int days
;;     - get days in range - calculate all streaks - find longest streak
;; [ ] days since last active - int
;; ...where "range" is a week, a month, a year, the entire range of records, or an arbitrary date range

;; details!
;; for each activity, spent most time on doing X (where X is a
;; comment, assuming you use comments to detail what you did)

;; Really might need emacs-async for this...buttloads of big
;; calculations which will only get bigger as the timelog file grows,
;; and the more the activities, the more the calculations! I'm
;; visualizing the table loading gradually, field by field, like an
;; image in a browser.

;; TODO -
;; 1. [x] show dash instead of zero
;; 2. [x] percent for active days
;; 3. buttons
;; 4. [x] display date ranges in a nicer way
;; 5. month and year ranges
;; 6. totals for each column
;; 7. (maybe) jump between chronometrist-report and chronometrist-statistics for viewing the same week's data
;;    - in chronometrist-statistics, this only makes sense in week mode
;; 8. a 'counter' - if I have ten weeks of data and I'm on the latest,
;;    show 10/10; update this as we scroll
;;    - don't scroll past the end, as currently happens
;;    - also applicable to chronometrist-report

;; TODO - convert all functions which take dates as arguments to use
;; the (YEAR MONTH DAY) format

(defun chronometrist-date->time (date)
  "Converts DATE to a time value (see (info \"(elisp)Time of Day\")).
DATE must be a list in the form (YEAR MONTH DAY)."
  (->> date (reverse) (apply #'encode-time 0 0 0)))

(defun chronometrist-date-less-p (date1 date2)
  "Like `time-less-p' but for dates. Returns ‘t’ if date1 is less
than date2. Both must be lists in the form (YEAR MONTH DAY)."
  (time-less-p (chronometrist-date->time date1) (chronometrist-date->time date2)))

(defun chronometrist-statistics-count-active-days (project &optional table)
  "Return the number of days the user spent a non-zero amount of
time on PROJECT, based on their `timeclock-file'. TABLE must be a
hash table - if not supplied, `chronometrist-events' is used.

This will not return correct results if TABLE contains records
which span midnights. (see `chronometrist-events-clean')"
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

(defun chronometrist-project-events-in-day (project date)
  "Get events for PROJECT on DATE. DATE must be in the form (YEAR MONTH DAY).

Returns a list of events, where each event is a vector in the
form [CODE YEAR MONTH DAY HOURS MINUTES SECONDS PROJECT-OR-COMMENT].

This will not return correct results if TABLE contains records
which span midnights. (see `chronometrist-events-clean')"
  (let ((save-next)
        (results))
    (seq-do (lambda (event)
              (cond ((and (equal "i" (chronometrist-vfirst event))
                          (equal project (chronometrist-vlast event)))
                     (->> event (list) (append results) (setq results))
                     (setq save-next t))
                    (save-next
                     (->> event (list) (append results) (setq results))
                     (setq save-next nil))
                    (t nil)))
            (gethash date chronometrist-events))
    results))

(defun chronometrist-events->time-list (events)
  (--map (pcase it
           (`[,_ ,year ,month ,day ,h ,m ,s ,_]
            (encode-time s m h day month year)))
         events))

(defun chronometrist-time-list->sum-of-intervals (time-values)
  "Takes a list of time values (see (info \"(elisp)Time of Day\")),
treats them as alternating start/end times, finds the intervals
between them, and adds the intervals to return a single time value."
  (->> (-partition 2 time-values)
       (--map (time-subtract (cadr it) (car it)))
       (-reduce #'time-add)))

(defun chronometrist-statistics-count-average-time-spent (project &optional table)
  "Return the average time the user has spent on PROJECT based on
their `timeclock-file'. TABLE must be a hash table - if not
supplied, `chronometrist-events' is used.

This will not return correct results if TABLE contains records
which span midnights. (see `chronometrist-events-clean')"
  (let ((table (if table table chronometrist-events))
        (days  0)
        (per-day-time-list))
    (maphash (lambda (key value)
               (let ((events-in-day (chronometrist-project-events-in-day project key)))
                 (when events-in-day
                   (setq days (1+ days))
                   (->> events-in-day
                        (chronometrist-events->time-list)
                        (chronometrist-time-list->sum-of-intervals)
                        (list)
                        (append per-day-time-list)
                        (setq per-day-time-list)))))
             table)
    (if per-day-time-list
        (--> per-day-time-list
             (-reduce #'time-add it)
             (cadr it)
             (/ it days))
      0)))

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
plist in the form (:mode :start :end).

:MODE is either 'week, 'month, 'year, 'full, or 'custom.

'week, 'month, and 'year mean display statistics
weekly/monthly/yearly respectively.

'full means display statistics from the beginning to the end of
the `timeclock-file'.

'custom means display statistics from an arbitrary date range.

:START and :END are the start and end of the date range to be
displayed. They must be dates in the form (YEAR MONTH DAY).")

(defvar chronometrist-statistics--point nil)

;; ## FUNCTIONS ##

(defun chronometrist-calendrical->date (date)
  "Convert calendrical information (see (info \"(elisp)Time of Day\"))
to a date in the form (YEAR MONTH DAY)."
  (-> date (-slice 3 6) (reverse)))

(defun chronometrist-statistics-entries-internal (table)
  "Helper function for `chronometrist-statistics-entries'."
  (mapcar (lambda (project)
            (let* ((active-days    (chronometrist-statistics-count-active-days project table))
                   (active-percent (case (plist-get chronometrist-statistics--ui-state :mode)
                                     ('week (* 100 (/ active-days 7.0)))))
                   (active-percent (if (zerop active-days)
                                       (format "% 6s" "-")
                                     (format "%05.2f%%" active-percent)))
                   (active-days    (format "% 5s" (if (zerop active-days)
                                                      "-"
                                                    active-days)))
                   (average-time   (->> (chronometrist-statistics-count-average-time-spent project table)
                                        (chronometrist-seconds-to-hms)
                                        (chronometrist-format-time)
                                        (format "% 5s")))
                   (content        (vector project
                                           active-days
                                           active-percent
                                           average-time)))
              (list project content)))
          timeclock-project-list))

(defun chronometrist-statistics-entries ()
  "Creates entries to be displayed in the buffer created by
`chronometrist-statistics', as specified by `tabulated-list-entries'."
  ;; We assume that all fields in `chronometrist-statistics--ui-state' are set, so they must
  ;; be changed by the view-changing functions.
  (case (plist-get chronometrist-statistics--ui-state :mode)
    ('week
     (let* ((start (plist-get chronometrist-statistics--ui-state :start))
            (end   (plist-get chronometrist-statistics--ui-state :end))
            (table (chronometrist-events-subset start end)))
       (chronometrist-statistics-entries-internal table)))
    (t ;; `chronometrist-statistics--ui-state' is nil, show current week's data
     (let* ((start-long (chronometrist-report-previous-week-start (decode-time)))
            (end-long   (chronometrist-date-op start-long '+ 7))
            (start      (chronometrist-calendrical->date start-long))
            (end        (chronometrist-calendrical->date end-long))
            (table      (chronometrist-events-subset start end)))
       (setq chronometrist-statistics--ui-state `(:mode week :start ,start :end ,end))
       (chronometrist-statistics-entries-internal table)))))

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

(defun chronometrist-statistics-format-date (date)
  (-let [(year month day) date]
    (format "%04d-%02d-%02d" year month day)))

(defun chronometrist-statistics-print-non-tabular ()
  "Print the non-tabular part of the buffer in `chronometrist-statistics'."
  (let ((w "\n    ")
        (inhibit-read-only t))
    (goto-char (point-max))
    (insert w)
    (insert-text-button (case (plist-get chronometrist-statistics--ui-state :mode)
                          ('week "Weekly view"))
                        ;; 'action #'chronometrist-report-previous-week ;; TODO - make interactive function to accept new mode from user
                        'follow-link t)
    (insert ", from")
    (insert
     (format " %s to %s"
             (chronometrist-statistics-format-date (plist-get chronometrist-statistics--ui-state :start))
             (chronometrist-statistics-format-date (plist-get chronometrist-statistics--ui-state :end))))))

(defun chronometrist-statistics-refresh ()
  (with-current-buffer chronometrist-statistics-buffer-name
    (let* ((w  (get-buffer-window chronometrist-statistics-buffer-name t))
           (wp (window-point w))
           (p  (point)))
      (timeclock-reread-log)
      (tabulated-list-print t nil)
      (chronometrist-statistics-print-non-tabular)
      ;; (chronometrist-statistics-maybe-start-timer)
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
        [("Project"      25 t)
         ("Active days"  12 t)
         ("Activity %"   10 t)
         ("Average time" 12 t)
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

If PRESERVE-STATE is nil (the default when not supplied), display
data from the current week. Otherwise, display data from the week
specified by `chronometrist-statistics--ui-state'."
  (interactive)
  (let* ((buffer (get-buffer-create chronometrist-statistics-buffer-name))
         (today  (decode-time))
         (week-start (chronometrist-calendrical->date
                      (chronometrist-report-previous-week-start today)))
         (week-end   (chronometrist-date-op week-start #'+ 6)))
    (with-current-buffer buffer
      (cond ((get-buffer-window chronometrist-statistics-buffer-name)
             (kill-buffer buffer))
            (t ;; (delete-other-windows)
             (when (not preserve-state)
               (setq chronometrist-statistics--ui-state `(:mode week
                                  :start ,week-start
                                  :end ,week-end)))
             (chronometrist-common-create-timeclock-file)
             (chronometrist-statistics-mode)
             (switch-to-buffer buffer)
             (chronometrist-statistics-refresh))))))

(defun chronometrist-statistics-previous-range (arg)
  "View the statistics in the previous time range."
  (interactive "P")
  (let* ((arg   (if (and arg (numberp arg))
                    (abs arg)
                  1))
         (start (plist-get chronometrist-statistics--ui-state :start))
         (end   (plist-get chronometrist-statistics--ui-state :end)))
    (case (plist-get chronometrist-statistics--ui-state :mode)
      ('week
       (let* ((new-start (chronometrist-date-op start     #'- (* 7 arg)))
              (new-end   (chronometrist-date-op new-start #'+ 6)))
         (plist-put chronometrist-statistics--ui-state :start new-start)
         (plist-put chronometrist-statistics--ui-state :end new-end))))
    (setq chronometrist-statistics--point (point))
    (kill-buffer)
    (chronometrist-statistics t)))

(defun chronometrist-statistics-next-range (arg)
  "View the statistics in the next time range."
  (interactive "P")
  (let* ((arg   (if (and arg (numberp arg))
                    (abs arg)
                  1))
         (start (plist-get chronometrist-statistics--ui-state :start))
         (end   (plist-get chronometrist-statistics--ui-state :end)))
    (case (plist-get chronometrist-statistics--ui-state :mode)
      ('week
       (let* ((new-start (chronometrist-date-op start     #'+ (* 7 arg)))
              (new-end   (chronometrist-date-op new-start #'+ 6)))
         (plist-put chronometrist-statistics--ui-state :start new-start)
         (plist-put chronometrist-statistics--ui-state :end new-end))))
    (setq chronometrist-statistics--point (point))
    (kill-buffer)
    (chronometrist-statistics t)))

(provide 'chronometrist-statistics)

;; Local Variables:
;; nameless-current-name: "chronometrist-statistics"
;; End:
