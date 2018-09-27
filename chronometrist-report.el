(require 'chronometrist-lib)
(require 'chronometrist-report-custom)

;; TODO - timers in chronometrist-report and chronometrist can
;; probably be merged into one function

;; TODO - improve first-run (no file, or no data in file) behaviour

;; TODO - add support for custom week start day to
;; tabulated-list-format. Have it use chronometrist-report-weekday-number-alist for day
;; names to aid i10n

;; TODO - add numeric arguments for next/previous week

;; TODO - use variables instead of hardcoded numbers to determine spacing

;; BUG - the timer for chronometrist-report doesn't currently seem to
;; be working. Not very high priority, as it's rare to see it with an
;; active project + to check it continuously for the latest + the
;; toggle behaviour already updates it.

;; ## TIMER ##

(defun chronometrist-report-timer ()
  (when (and (chronometrist-buffer-exists? chronometrist-report-buffer-name)
             (chronometrist-buffer-visible? chronometrist-report-buffer-name))
    (timeclock-reread-log)
    (with-current-buffer chronometrist-report-buffer-name
      (let ((position (point)))
        (tabulated-list-print t nil)
        (chronometrist-report-print-non-tabular)
        (goto-char position)))))

(defun chronometrist-report-maybe-start-timer ()
  (unless chronometrist-report--timer-object
    (setq chronometrist-report--timer-object
          (run-at-time t chronometrist-report-update-interval #'chronometrist-timer))))

(defvar chronometrist-report--timer-object nil)

(defun chronometrist-change-update-interval (arg)
  (interactive "NEnter new interval (in seconds): ")
  (cancel-timer chronometrist-report--timer-object)
  (setq chronometrist-report--update-interval arg
        chronometrist-report--timer-object nil)
  (chronometrist-report-maybe-start-timer))

;; ## FUNCTIONS ##

(defvar chronometrist-report--ui-date
  nil
  "The first date of the week displayed by
`chronometrist-report' (specifically `chronometrist-report-entries'). A
value of nil means the current week. Otherwise, it must be a list
in the form (YEAR WEEK), where WEEK is the numeric week of the
year (1-52).")

(defvar chronometrist-report--ui-week-dates
  nil
  "List of dates currently displayed by
`chronometrist-report' (specifically `chronometrist-report-entries').
Each date is a list containing calendrical information (see (info \"(elisp)Time Conversion\"))")

(defun chronometrist-report-day-of-week->number (day-of-week)
  (cdr
   (assoc-string day-of-week chronometrist-report-weekday-number-alist)))

(defun chronometrist-report-increment-or-decrement-date (date operator &optional count)
  "Return DATE incremented or decremented by COUNT days (1 if not
supplied).

DATE must be calendrical information (see (info \"(elisp)Time Conversion\"))

OPERATOR must be either '+ or '-

COUNT must be a positive integer."
  (let ((seconds   (elt date 0))
        (minutes   (elt date 1))
        (hours     (elt date 2))
        (day       (elt date 3))
        (month     (elt date 4))
        (year      (elt date 5))
        (count     (if count count 1)))
    (-->
     (encode-time seconds minutes hours day month year)
     (funcall (cond ((equal operator '+) 'time-add)
                    ((equal operator '-) 'time-subtract)
                    (t (error "Unknown operator %s" operator)))
              it (list 0 (* 86400 count)))
     (decode-time it))))

(defun chronometrist-report-previous-week-start (date)
  "Return the date for the last start-of-week from DATE (using
start-of-week defined in `chronometrist-report-week-start-day'). If
the day of DATE is the same as the
`chronometrist-report-week-start-day', return DATE.

DATE must be calendrical information (see (info \"(elisp)Time Conversion\")).

Any time data provided is reset to midnight (00:00:00)."
  (let* ((date       (->> date
                          (-drop 3)
                          (append '(0 0 0))))
         (day        (elt date 6)) ;; 0-6, where 0 = Sunday
         (week-start (chronometrist-report-day-of-week->number chronometrist-report-week-start-day))
         (gap        (cond ((> day week-start) (- day week-start))
                           ((< day week-start) (+ day (- 7 week-start))))))
    (if gap
        (chronometrist-report-increment-or-decrement-date date '- gap)
      date)))

(defun chronometrist-report-date ()
  "Return the date specified by `chronometrist-report--ui-date'. If
it is nil, return the current date as calendrical
information (see (info \"(elisp)Time Conversion\"))."
  (if chronometrist-report--ui-date chronometrist-report--ui-date (decode-time)))

(defun chronometrist-report-date->dates-in-week (first-date-in-week)
  "Return a list in the form (DAY-1 DAY-2 ... DAY-7), where each
day is a time value (see (info \"(elisp)Time of Day\")).

FIRST-DATE-IN-WEEK must be a time value representing DAY-1."
  (--> '(0 1 2 3 4 5 6)
       ;; 1 day = 86400 seconds
       (--map (* 86400 it) it)
       (--map (list
               (car first-date-in-week)
               (+ (cadr first-date-in-week) it))
              it)))

(defun chronometrist-report-dates-in-week->string (dates-in-week)
  "Return a list in the form (DAY-1 DAY-2 ... DAY-7), where each
day is a string in the form \"YYYY/MM/DD\""
  (--map (format "%04d/%02d/%02d"
                 (elt it 5)
                 (elt it 4)
                 (elt it 3))
         dates-in-week))

(defun chronometrist-report-date->week-dates ()
  "Return dates in week as a list, where each element is
calendrical information (see (info \"(elisp)Time Conversion\")).
The first date is the first occurrence of
`chronometrist-report-week-start-day' before the date specified in
`chronometrist-report--ui-date' (if non-nil) or the current date."
  (->> (chronometrist-report-date)
       (chronometrist-report-previous-week-start)
       (-take 6)
       (apply #'encode-time)
       (chronometrist-report-date->dates-in-week)
       (-map #'decode-time)))

(defun chronometrist-report-entries ()
  "Creates entries to be displayed in the buffer created by
`chronometrist-report'."
  (let* ((week-dates        (chronometrist-report-date->week-dates))
         (week-dates-string (chronometrist-report-dates-in-week->string week-dates)))
    (setq chronometrist-report--ui-week-dates week-dates)
    (mapcar (lambda (project)
              (let ((project-daily-time-list (--map (chronometrist-project-time-one-day project it) week-dates)))
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

(defun chronometrist-report-format-date (format-string time-date)
  "Extract date from TIME-DATE and format it according to
FORMAT-STRING."
  (->> time-date
       (-take 6)
       (-drop 3)
       (reverse)
       (apply #'format format-string)))

(defun chronometrist-report-print-non-tabular ()
  "Print the non-tabular part of the buffer in `chronometrist-report'."
  (let ((inhibit-read-only t)
        (w "\n    "))
    (goto-char (point-min))
    (insert "                         ")
    (--map (insert (chronometrist-report-format-date "%04d-%02d-%02d " it))
           (chronometrist-report-date->week-dates))
    (insert "\n")
    (goto-char (point-max))
    (insert w (format "%- 21s" "Total"))
    (let ((total-time-daily (->> chronometrist-report--ui-week-dates
                                 (mapcar #'chronometrist-total-time-one-day))))
      (->> total-time-daily
           (mapcar #'chronometrist-format-time)
           (--map (format "% 9s  " it))
           (apply #'insert))
      (->> total-time-daily
           (-reduce #'chronometrist-time-add)
           (chronometrist-format-time)
           (format "% 13s")
           (insert)))
    (insert "\n" w "l - open log file")))

;; ## MAJOR MODE ##

(define-derived-mode chronometrist-report-mode tabulated-list-mode "Chronometrist-Report"
  "Major mode for `chronometrist-report'."
  (timeclock-reread-log)

  (make-local-variable 'tabulated-list-format)
  (setq tabulated-list-format [("Project"   25 t)
                               ("Sunday"    10 t)
                               ("Monday"    10 t)
                               ("Tuesday"   10 t)
                               ("Wednesday" 10 t)
                               ("Thursday"  10 t)
                               ("Friday"    10 t)
                               ("Saturday"  10 t :pad-right 5)
                               ("Total"     12 t)])

  (make-local-variable 'tabulated-list-entries)
  (setq tabulated-list-entries 'chronometrist-report-entries)

  (make-local-variable 'tabulated-list-sort-key)
  (setq tabulated-list-sort-key '("Project" . nil))

  (tabulated-list-init-header)

  (chronometrist-report-maybe-start-timer)
  (define-key chronometrist-report-mode-map (kbd "l") #'chronometrist-open-timeclock-file)
  (define-key chronometrist-report-mode-map (kbd "b") #'chronometrist-report-previous-week)
  (define-key chronometrist-report-mode-map (kbd "f") #'chronometrist-report-next-week))

;; ## COMMANDS ##

(defun chronometrist-report (&optional keep-date)
  "Display a weekly report of the user's timeclock.el projects
and the time spent on them each day, based on their timelog file
in `timeclock-file'. This is the 'listing command' for
chronometrist-report-mode.

If KEEP-DATE is nil (the default when not supplied), set
`chronometrist-report--ui-date' to nil and display data from the
current week. Otherwise, display data from the week specified by
`chronometrist-report--ui-date'."
  (interactive)
  (let ((buffer (get-buffer-create chronometrist-report-buffer-name)))
    ;; we want this command to toggle viewing the report
    (if (and (chronometrist-buffer-visible? chronometrist-report-buffer-name)
             (not keep-date))
        (kill-buffer buffer)
      (with-current-buffer buffer
        (delete-other-windows)
        (when (not keep-date)
          (setq chronometrist-report--ui-date nil))
        (chronometrist-common-create-timeclock-file)
        (chronometrist-report-mode)
        (tabulated-list-print)
        (chronometrist-report-print-non-tabular)
        (switch-to-buffer buffer)))))

(defun chronometrist-report-previous-week (arg)
  "View the previous week's report."
  (interactive "P")
  (let ((arg (if (and arg (numberp arg))
                 (abs arg)
               1)))
    (if chronometrist-report--ui-date
        (setq chronometrist-report--ui-date
              (chronometrist-report-increment-or-decrement-date chronometrist-report--ui-date '- (* 7 arg)))
      (setq chronometrist-report--ui-date
            (chronometrist-report-increment-or-decrement-date (decode-time) '- (* 7 arg))))
    (kill-buffer)
    (chronometrist-report t)))

(defun chronometrist-report-next-week (arg)
  "View the next week's report."
  (interactive "P")
  (let ((arg (if (and arg (numberp arg))
                 (abs arg)
               1)))
    (if chronometrist-report--ui-date
        (setq chronometrist-report--ui-date
              (chronometrist-report-increment-or-decrement-date chronometrist-report--ui-date '+ (* 7 arg)))
      (setq chronometrist-report--ui-date
            (chronometrist-report-increment-or-decrement-date (decode-time) '+ (* 7 arg))))
    (kill-buffer)
    (chronometrist-report t)))

(provide 'chronometrist-report)

;; Local Variables:
;; nameless-current-name: "chronometrist-report"
;; End:
