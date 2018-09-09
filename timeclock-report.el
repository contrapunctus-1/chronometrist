(require 'timeclock-ui-lib)

;; New week logic
;; 1. ✓ get current date in calendrical form (decode-time)
;; 2. ✓ check if we're on a Sunday/user-specified week start day, else
;;    decrement till we are (timeclock-report-previous-week-start)
;; 3. set global state to that date (`timeclock-report-week-start-day')
;; 4. decrement/increment global date by 7 to get previous/next week
;; 5. get week's dates using global date

;; TODO - add support for custom week start day to
;; tabulated-list-format. Have it use timeclock-report-weekday-number-alist for day
;; names to aid i10n

(defvar timeclock-report-week-start-day "Sunday"
  "The day used for start of week by `timeclock-report'.")
(defvar timeclock-report--ui-date
  nil
  "The first date of the week displayed by
`timeclock-report' (specifically `timeclock-report-entries'). A
value of nil means the current week. Otherwise, it must be a list
in the form (YEAR WEEK), where WEEK is the numeric week of the
year (1-52).")

(defvar timeclock-report-weekday-number-alist
  '(("Sunday"    . 0)
    ("Monday"    . 1)
    ("Tuesday"   . 2)
    ("Wednesday" . 3)
    ("Thursday"  . 4)
    ("Friday"    . 5)
    ("Saturday"  . 6))
  "alist in the form (\"NAME\" . NUMBER), where \"NAME\" is the
  name of a weekday and NUMBER its associated number.")

(defun timeclock-report-day-of-week->number (day-of-week)
  (cdr
   (assoc-string day-of-week timeclock-report-weekday-number-alist)))

(defun timeclock-report-increment-or-decrement-date (date operator &optional count)
  "Increment or decrement DATE by COUNT days (1 if not supplied).

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

(defun timeclock-report-previous-week-start (date)
  "Return the date for the last start-of-week from DATE (using
start-of-week defined in `timeclock-report-week-start-day'). If
the day of DATE is the same as the
`timeclock-report-week-start-day', return DATE.

DATE must be calendrical information (see (info \"(elisp)Time Conversion\")).

Any time data provided is reset to midnight (00:00:00)."
  (let* ((date       (->> date
                          (-drop 3)
                          (append '(0 0 0))))
         (day        (elt date 6)) ;; 0-6, where 0 = Sunday
         (week-start (timeclock-report-day-of-week->number timeclock-report-week-start-day))
         (gap        (cond ((> day week-start) (- day week-start))
                           ((< day week-start) (+ day (- 7 week-start))))))
    (if gap
        (timeclock-report-increment-or-decrement-date date '- gap)
      date)))

(defun timeclock-report-date ()
  "Return the date specified by `timeclock-report--ui-date'. If
it is nil, return the current date as calendrical
information (see (info \"(elisp)Time Conversion\"))."
  (if timeclock-report--ui-date
      timeclock-report--ui-date
    (decode-time)))

;; ## VARIABLES ##
(defvar timeclock-report-buffer-name "*Timeclock-Report*")
(defvar timeclock-report--year-week
  nil
  "Variable to determine the week displayed by
`timeclock-report' (specifically `timeclock-report-entries'). A value of nil
means the current week (starting from Sunday). Otherwise, it must
be a list in the form (YEAR WEEK), where WEEK is the numeric week
of the year (1-52).")

;; ## FUNCTIONS ##

(defun timeclock-report-week->date (year week)
  "Return the date as a list in the form (YEAR MONTH DAY) of the
first day of the WEEK in YEAR, where WEEK is a week
number (1-52)."
  (let ((day    (* week 7))
        (month  1))
    (while (> day 31)
      (setq day (- day (calendar-last-day-of-month month year))
            month (1+ month)))
    (list year month day)))

(defun timeclock-report-current-week ()
  "Return current week as a number (1-52)."
  (string-to-number
   (format-time-string "%U")))

(defun timeclock-report-week ()
  "Return current week from `timeclock-report-current-week' or the week
specified in `timeclock-report--year-week'."
  (if timeclock-report--year-week
      (cadr timeclock-report--year-week)
    (timeclock-report-current-week)))

(defun timeclock-report-year ()
  "Return current year, or the year specified in
`timeclock-report--year-week'."
  (if timeclock-report--year-week
      (car timeclock-report--year-week)
    (elt (decode-time) 5)))

;; maybe these two should take two arguments instead of a list?
(defun timeclock-report-dec-year-week (year-week)
  "Decrements YEAR-WEEK by one week. YEAR-WEEK must be a list in
the form (YEAR WEEK), where WEEK is the numeric week in
YEAR (1-52)."
  (let ((y (car year-week))
        (w (cadr year-week)))
    (if (= w 1)
        (list (1- y) 52)
      (list y (1- w)))))

(defun timeclock-report-inc-year-week (year-week)
  "Increments YEAR-WEEK by one week. YEAR-WEEK must be a list in
the form (YEAR WEEK), where WEEK is the numeric week in
YEAR (1-52)."
  (let ((y (car year-week))
        (w (cadr year-week)))
    (if (= w 52)
        (list (1+ y) 1)
      (list y (1+ w)))))

(defun timeclock-report-date->dates-in-week (first-date-in-week)
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

(defun timeclock-report-entries ()
  "Creates entries to be displayed in the buffer created by
`timeclock-report'."
  (let* ((date               (timeclock-report-date))
         (first-date-of-week (->> date
                                  (timeclock-report-previous-week-start)
                                  (-take 6)
                                  (apply #'encode-time)))
         ;; list of dates of each day in WEEK
         (dates-in-week      (->> (timeclock-report-date->dates-in-week first-date-of-week)
                                  (-map #'decode-time)
                                  (--map (format "%02d/%02d/%02d"
                                                 (elt it 5)
                                                 (elt it 4)
                                                 (elt it 3))))))
    (mapcar (lambda (project)
              (list project
                    (vconcat
                     (vector project)
                     (apply #'vector
                            (--map (timeclock-ui-format-time
                                    (timeclock-ui-project-time-one-day project it))
                                   dates-in-week)))))
            timeclock-project-list)))

(defun timeclock-report-idle-timer ()
  (when (and (timeclock-ui-buffer-exists? timeclock-report-buffer-name)
             (timeclock-ui-buffer-visible? timeclock-report-buffer-name))
    (timeclock-reread-log)
    (with-current-buffer timeclock-report-buffer-name
      (tabulated-list-print t t))))

(defun timeclock-report-format-date (date)
  (->> date
       (-take 6)
       (-drop 3)
       (reverse)
       (apply #'format "%02d-%02d-%02d")))

(defun timeclock-report-print-non-tabular ()
  "Print the non-tabular part of the buffer in `timeclock-report'."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (-->
     ;; (timeclock-list-total-time-one-day)
     ;; (timeclock-ui-format-time it)
     ;; (format "\n    %- 26s%s" "Total" it)
     ""
     (concat it
             "\n\n    l - open log file")
     (insert it))
    (insert  "\n\n    "
             (if timeclock-report--ui-date
                 (timeclock-report-format-date timeclock-report--ui-date)
               (timeclock-report-format-date (decode-time))))))

;; ## MAJOR MODE ##

(define-derived-mode timeclock-report-mode tabulated-list-mode "Timeclock-Report"
  "Major mode for `timeclock-report'."
  (timeclock-reread-log)

  (make-local-variable 'tabulated-list-format)
  (setq tabulated-list-format [("Project"   25 t)
                               ("Sunday"    10 t)
                               ("Monday"    10 t)
                               ("Tuesday"   10 t)
                               ("Wednesday" 10 t)
                               ("Thursday"  10 t)
                               ("Friday"    10 t)
                               ("Saturday"  10 t)])

  (make-local-variable 'tabulated-list-entries)
  (setq tabulated-list-entries 'timeclock-report-entries)

  (make-local-variable 'tabulated-list-sort-key)
  (setq tabulated-list-sort-key '("Project" . nil))

  (tabulated-list-init-header)

  (run-with-idle-timer 3 t #'timeclock-report-idle-timer)
  (define-key timeclock-report-mode-map (kbd "l") 'timeclock-list-open-timeclock-file)
  (define-key timeclock-report-mode-map (kbd "b") 'timeclock-report-previous-week)
  (define-key timeclock-report-mode-map (kbd "f") 'timeclock-report-next-week))

;; ## COMMANDS ##

(defun timeclock-report (&optional keep-week)
  "Display a weekly report of the user's timeclock.el projects
and the time spent on them each day, based on their timelog file
in `timeclock-file'. This is the 'listing command' for
timeclock-report-mode.

If KEEP-WEEK is nil (the default when not supplied), set
`timeclock-report--year-week' to nil and display data from the
current week. Otherwise, display data from the week specified by
`timeclock-report--year-week'."
  (interactive)
  (let ((buffer (get-buffer-create timeclock-report-buffer-name)))
    ;; we want this command to toggle viewing the report
    (if (and (timeclock-ui-buffer-visible? timeclock-report-buffer-name)
             (not keep-week))
        (kill-buffer buffer)
      (with-current-buffer buffer
        (delete-other-windows)
        (when (not keep-week)
          (setq timeclock-report--ui-date nil))
        (timeclock-report-mode)
        (tabulated-list-print)
        (timeclock-report-print-non-tabular)
        (switch-to-buffer buffer)))))

(defun timeclock-report-previous-week ()
  "View the previous week's report."
  (interactive)
  (if timeclock-report--ui-date
      (setq timeclock-report--ui-date
            (timeclock-report-increment-or-decrement-date timeclock-report--ui-date '- 7))
    (setq timeclock-report--ui-date
          (timeclock-report-increment-or-decrement-date (decode-time) '- 7)))
  (kill-buffer)
  (timeclock-report t))

(defun timeclock-report-next-week ()
  "View the next week's report."
  (interactive)
  (if timeclock-report--ui-date
      (setq timeclock-report--ui-date
            (timeclock-report-increment-or-decrement-date timeclock-report--ui-date '+ 7))
    (setq timeclock-report--ui-date
          (timeclock-report-increment-or-decrement-date (decode-time) '+ 7)))
  (kill-buffer)
  (timeclock-report t))

(provide 'timeclock-report)
