(require 'chronometrist-lib)
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

(defvar chronometrist-events (make-hash-table :test #'equal))

(defun chronometrist-vfirst (vector)
  "Return the first element of VECTOR."
  (elt vector 0))

(defun chronometrist-vlast (vector)
  "Return the last element of VECTOR."
  (elt vector (1- (length vector))))

;; test function
(defun chronometrist-list-midnight-spanning-events ()
  (let ((dates))
    (maphash (lambda (key value)
               (when (-> value (chronometrist-vfirst) (chronometrist-vfirst) (equal "o"))
                 (->> key (list) (append dates) (setq dates))))
             chronometrist-events)
    dates))

(defun chronometrist-clean-ht ()
  "Clean `chronometrist-events' by splitting intervals which span
midnights into two. For event data to be processed accurately,
this must be called after `chronometrist-populate-ht'. Returns t
if the table was modified, else nil."
  ;; for each key-value, see if the first event has an "o" code
  (let ((prev-date)
        (modified))
    (maphash (lambda (key value)
               (when (-> value (chronometrist-vfirst) (chronometrist-vfirst) (equal "o"))
                 ;; Add new "o" event on previous date with 24:00:00
                 ;; as end time, reusing the ending reason.
                 ;; Add new "i" event on current date with 00:00:00
                 ;; as start time, with the same project.
                 (let* ((reason  (->> value (chronometrist-vfirst) (chronometrist-vlast)))
                        (prev-events (gethash prev-date chronometrist-events))
                        (prev-event  (chronometrist-vlast prev-events))
                        (o-event     (vconcat ["o"] prev-date `[24 0 0 ,reason]))

                        (current-event (chronometrist-vfirst value))
                        (project       (chronometrist-vlast prev-event))
                        (i-event       (vconcat ["i"] key `[0 0 0 ,project])))
                   (--> prev-events
                        (vconcat it (vector o-event))
                        (puthash prev-date it chronometrist-events))
                   (--> (vconcat (vector i-event) value)
                        (puthash key it chronometrist-events))
                   (setq modified t)))
               (setq prev-date key))    ; this assumes that the first event of the first date doesn't
                                        ; have an "o" code (which a correct file shouldn't)
             chronometrist-events)
    modified))

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

(defun chronometrist-statistics-count-project-days (project &optional table)
  "Return the number of days the user worked on PROJECT based on
their `timeclock-file'. TABLE must be a hash table - if not
supplied, `chronometrist-events' is used.

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

(defun chronometrist-subset-ht (start-key end-key table)
  "Return a subset of hash table TABLE, containing values between
START-KEY and END-KEY. START-KEY and END-KEY must be keys in
TABLE."
  (let ((subset)
        (match-area))
    (maphash (lambda (key value)
               (if (equal key start-key))))))

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

(defvar chronometrist-statistics--ui-mode nil
  "The display mode for `chronometrist-statistics'. Valid values are
'week, 'month, 'year, 'full, or 'custom.

'week, 'month, and 'year mean display statistics
weekly/monthly/yearly respectively.

'full means display statistics from the beginning to the end of
the `timeclock-file'.

'custom means display statistics from an arbitrary date range.")

(defvar chronometrist-statistics--ui-date nil
  "The first date of the week displayed by `chronometrist-statistics' (specifically `chronometrist-statistics-entries').
A value of nil means the current week. Otherwise, it must be a
list in the form (YEAR WEEK), where WEEK is the numeric week of
the year (1-52).")

(defvar chronometrist-statistics--ui-week-dates nil
  "List of dates currently displayed by `chronometrist-statistics' (specifically `chronometrist-statistics-entries').
Each date is a list containing calendrical information (see (info \"(elisp)Time Conversion\"))")

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
         ("Days worked" 10 t)
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

(defun chronometrist-statistics (&optional keep-date)
  "Display statistics of the user's timeclock.el projects, based
on their timelog file in `timeclock-file'. This is the 'listing
command' for chronometrist-statistics-mode.

If a buffer called `chronometrist-statistics-buffer-name' already
exists and is visible, kill the buffer.

If KEEP-DATE is nil (the default when not supplied), set
`chronometrist-statistics--ui-date' to nil and display data from the
current week. Otherwise, display data from the week specified by
`chronometrist-statistics--ui-date'."
  (interactive)
  (let ((buffer (get-buffer-create chronometrist-statistics-buffer-name)))
    (with-current-buffer buffer
      (cond ((get-buffer-window chronometrist-statistics-buffer-name)
             (kill-buffer buffer))
            (t ;; (delete-other-windows)
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
    (if chronometrist-statistics--ui-date
        (setq chronometrist-statistics--ui-date
              (chronometrist-statistics-increment-or-decrement-date chronometrist-statistics--ui-date '- (* 7 arg)))
      (setq chronometrist-statistics--ui-date
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
    (if chronometrist-statistics--ui-date
        (setq chronometrist-statistics--ui-date
              (chronometrist-statistics-increment-or-decrement-date chronometrist-statistics--ui-date '+ (* 7 arg)))
      (setq chronometrist-statistics--ui-date
            (chronometrist-statistics-increment-or-decrement-date (decode-time) '+ (* 7 arg))))
    (setq chronometrist-statistics--point (point))
    (kill-buffer)
    (chronometrist-statistics t)))

(provide 'chronometrist-statistics)

;; Local Variables:
;; nameless-current-name: "chronometrist-statistics"
;; End:
