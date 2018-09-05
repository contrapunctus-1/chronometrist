(require 'timeclock-ui-lib)

;; ## BUGS ##
;; 1. (tcr/week->date) needs a re-look - doesn't always start on a
;;    Sunday, and sometimes misses days.
;;      (tcr/week->date 2017 52) => (2017 12 30) (30th Dec 2017)
;;      (tcr/week->date 2018 1) => (2018 1 7) (7th Jan 2018)

;;    So if you're recreating a calendar using it, which we sort of are...
;;      (defun year-week->week-dates (year week)
;;        (->>
;;         (tcr/week->date year week)
;;         (reverse)
;;         (append '(0 0 0))
;;         (apply #'encode-time) ;; convert 'calendrical data' to time value
;;         (tcr/date->dates-in-week)
;;         (--map (decode-time it))
;;         (--map (format "%02d/%02d/%02d"
;;                        (elt it 5)
;;                        (elt it 4)
;;                        (elt it 3)))))
;;
;;      (year-week->week-dates 2017 52)
;;      => ("2017/12/30" "2017/12/31" "2018/01/01" "2018/01/02" "2018/01/03" "2018/01/04" "2018/01/05")
;;      (year-week->week-dates 2018 1)
;;      ("2018/01/07" "2018/01/08" "2018/01/09" "2018/01/10" "2018/01/11" "2018/01/12" "2018/01/13")
;;
;;      ...we missed 6th Jan 2018!
;;
;;      This is because we're assuming the year has exactly 52 weeks,
;;      but it has 52 weeks and 1 day (2 days in a leap year).

;; ## VARIABLES ##
(defvar timeclock-report-buffer-name "*Timeclock-Report*")
(defvar --timeclock-report-year-week
  nil
  "Variable to determine the week displayed by
`timeclock-report' (specifically `tcr/entries'). A value of nil
means the current week (starting from Sunday). Otherwise, it must
be a list in the form (YEAR WEEK), where WEEK is the numeric week
of the year (1-52).")

;; ## FUNCTIONS ##

;; Bit weird - (tcr/week->date 2018 1) => (2018 1 7) (7th of Jan 2018)
;; Although that _is_ the first date of the first complete week
;; (starting on Sunday) in 2018...
(defun tcr/week->date (year week)
  "Return the date as a list in the form (YEAR MONTH DAY) of the
first day of the WEEK in YEAR, where WEEK is a week
number (1-52)."
  (let ((day    (* week 7))
        (month  1))
    (while (> day 31)
      (setq day (- day (calendar-last-day-of-month month year))
            month (1+ month)))
    (list year month day)))

(defun tcr/current-week ()
  "Return current week as a number (1-52)."
  (string-to-number
   (format-time-string "%U")))

(defun tcr/week ()
  "Return current week from `tcr/current-week' or the week
specified in `--timeclock-report-year-week'."
  (if --timeclock-report-year-week
      (cadr --timeclock-report-year-week)
    (tcr/current-week)))

(defun tcr/year ()
  "Return current year, or the year specified in
`--timeclock-report-year-week'."
  (if --timeclock-report-year-week
      (car --timeclock-report-year-week)
    (elt (decode-time) 5)))

;; maybe these two should take two arguments instead of a list?
(defun tcr/dec-year-week (year-week)
  "Decrements YEAR-WEEK by one week. YEAR-WEEK must be a list in
the form (YEAR WEEK), where WEEK is the numeric week in
YEAR (1-52)."
  (let ((y (car year-week))
        (w (cadr year-week)))
    (if (= w 1)
        (list (1- y) 52)
      (list y (1- w)))))

(defun tcr/inc-year-week (year-week)
  "Increments YEAR-WEEK by one week. YEAR-WEEK must be a list in
the form (YEAR WEEK), where WEEK is the numeric week in
YEAR (1-52)."
  (let ((y (car year-week))
        (w (cadr year-week)))
    (if (= w 52)
        (list (1+ y) 1)
      (list y (1+ w)))))

;; first-day-of-week and dates-in-week can be refactored
(defun tcr/entries ()
  "Creates entries to be displayed in the buffer created by
`timeclock-report'. WEEK should be a string containing the week
of the year (01-52)."
  (let* ((week (tcr/week))
         (year (tcr/year))
         ;; we need the time value to add to it
         (first-day-of-week (--> (tcr/week->date year week)
                                 (reverse it)
                                 (append '(0 0 0) it)
                                 (apply #'encode-time it)))
         ;; list of dates of each day in WEEK
         (dates-in-week      (--> '(0 1 2 3 4 5 6)
                                  ;; 1 day = 86400 seconds
                                  (--map (* 86400 it) it)
                                  (--map (list
                                          (car first-day-of-week)
                                          (+ (cadr first-day-of-week) it))
                                         it)
                                  (--map (decode-time it) it)
                                  (--map (format "%02d/%02d/%02d"
                                                 (elt it 5)
                                                 (elt it 4)
                                                 (elt it 3))
                                         it))))
    (mapcar (lambda (project)
              (list project
                    (vconcat
                     (vector project)
                     (apply #'vector
                            (--map (tcl/format-time
                                    (tcl/project-time-one-day project it))
                                   dates-in-week)))))
            timeclock-project-list)))

(defun tcr/idle-timer ()
  (when (and (tcl/buffer-exists? timeclock-report-buffer-name)
             (tcl/buffer-visible? timeclock-report-buffer-name))
    (timeclock-reread-log)
    (with-current-buffer timeclock-report-buffer-name
      (tabulated-list-print t t))))

(defun tcr/print-non-tabular ()
  "Print the non-tabular part of the buffer in `timeclock-report'."
  (let ((inhibit-read-only t))
    (goto-char (point-max))
    (-->
     ;; (tcl/total-time-one-day)
     ;; (tcl/format-time it)
     ;; (format "\n    %- 26s%s" "Total" it)
     ""
     (concat it
             "\n\n    l - open log file")
     (insert it))
    (insert "\n\n    Week "
            (number-to-string (tcr/week))
            " of 52, "
            (number-to-string (tcr/year)))))

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
  (setq tabulated-list-entries 'tcr/entries)

  (make-local-variable 'tabulated-list-sort-key)
  (setq tabulated-list-sort-key '("Project" . nil))

  (tabulated-list-init-header)

  (run-with-idle-timer 3 t #'tcr/idle-timer)
  (define-key timeclock-report-mode-map (kbd "l") 'tcl/open-timeclock-file)
  (define-key timeclock-report-mode-map (kbd "b") 'tcr/previous-week)
  (define-key timeclock-report-mode-map (kbd "f") 'tcr/next-week))

;; ## COMMANDS ##

(defun timeclock-report (&optional keep-week)
  "Displays a weekly report of the user's timeclock.el projects
and the time spent on them each day, based on their timelog file
in `timeclock-file'. This is the 'listing command' for
timeclock-report-mode.

If KEEP-WEEK is nil (the default when not supplied), we set
`--timeclock-report-year-week' to nil so that this command
displays data from the current week."
  (interactive)
  (let ((buffer (get-buffer-create timeclock-report-buffer-name)))
    ;; we want this command to toggle viewing the report
    (if (and (tcl/buffer-visible? timeclock-report-buffer-name)
             (not keep-week))
        (kill-buffer buffer)
      (with-current-buffer buffer
        (delete-other-windows)
        (when (not keep-week)
          (setq --timeclock-report-year-week nil))
        (timeclock-report-mode)
        (tabulated-list-print)
        (tcr/print-non-tabular)
        (switch-to-buffer buffer)))))

(defun tcr/previous-week ()
  "View the previous week's report."
  (interactive)
  (if --timeclock-report-year-week
      (setq --timeclock-report-year-week
            (tcr/dec-year-week --timeclock-report-year-week))
    (setq --timeclock-report-year-week
          (tcr/dec-year-week (list (tcr/year) (tcr/week)))))
  (kill-buffer)
  (timeclock-report t))

(defun tcr/next-week ()
  "View the next week's report."
  (interactive)
  (if --timeclock-report-year-week
      (setq --timeclock-report-year-week
            (tcr/inc-year-week --timeclock-report-year-week))
    (setq --timeclock-report-year-week
          (tcr/inc-year-week (list (tcr/year) (tcr/week)))))
  (kill-buffer)
  (timeclock-report t))

(provide 'timeclock-report)
