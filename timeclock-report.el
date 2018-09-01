(require 'cp-timeclock)

;; TODO
;; 1. Previous/next week
;; 2. Highlight current day
;; 3. If hours and minutes aren't used, don't print them
;; 4. Use - instead of "00:00:00"
;; 5. Add total time clocked per day

;; ## VARIABLES ##
(defvar timeclock-report-buffer-name "*Timeclock-Report")

;; ## FUNCTIONS ##

;; Bit weird - (tr/week->date 1 2018) => (7 1) (7th of Jan)
;; Although that _is_ the first date of the first complete week
;; (starting on Sunday) in 2018...
(defun tr/week->date (year week)
  "Get the date of the first day of the WEEK in YEAR, where WEEK
is a week number (01-52)."
  (let ((day    (* week 7))
        (month  1))
    (while (> day 31)
      (setq day (- day (calendar-last-day-of-month month year))
            month (1+ month)))
    (list year month day)))

(defun tr/entries (&optional year week)
  "Creates entries to be displayed in the buffer created by
`timeclock-report'. WEEK should be a string containing the week
of the year (01-52)."
  (let* ((week           (if week week
                           (string-to-number
                            (format-time-string "%U"))))
         (year           (if year year
                           (elt (decode-time) 5)))
         ;; we need the time value to add to it
         (first-day-of-week (--> (tr/week->date year week)
                                 (reverse it)
                                 (append '(0 0 0) it)
                                 (apply #'encode-time it)))
         ;; list of dates of each day in WEEK
         (dates-in-week      (--> '(0 1 2 3 4 5 6)
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
                            (--map (tclist/project-time-one-day project it)
                                   dates-in-week)))))
            timeclock-project-list)))

(defun tr/idle-timer ()
  (when (and (tclist/buffer-exists? timeclock-list-buffer-name)
             (tclist/buffer-visible? timeclock-list-buffer-name))
    (timeclock-reread-log)
    (with-current-buffer timeclock-list-buffer-name
      (tabulated-list-print t t))))

;; TODO - add support for other locale weeks/weekday names,
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
  (setq tabulated-list-entries 'tr/entries)

  (make-local-variable 'tabulated-list-sort-key)
  (setq tabulated-list-sort-key '("Project" . nil))

  (tabulated-list-init-header)

  (run-with-idle-timer 3 t #'tr/idle-timer)
  (define-key timeclock-list-mode-map (kbd "l") 'tclist/open-timeclock-file))

;; ## COMMANDS ##

(defun timeclock-report ()
  "Displays a weekly report of the user's timeclock.el projects
and the time spent on them each day, based on their timelog file
in `timeclock-file'. This is the 'listing command' for
timeclock-report-mode."
  (interactive)
  (let ((buffer (get-buffer-create timeclock-report-buffer-name)))
    ;; we want this command to toggle viewing the report
    (if (tclist/buffer-visible? timeclock-report-buffer-name)
        (kill-buffer timeclock-report-buffer-name)
      (with-current-buffer buffer
        ;; (setq buffer-read-only nil)
        (timeclock-report-mode)
        (tabulated-list-print)
        ;; using switch-to-buffer instead until we can preserve
        ;; position of point across successive calls to timeclock-list
        ;; (switch-to-buffer-other-window buffer)
        (switch-to-buffer buffer)))))

(provide 'timeclock-report)
