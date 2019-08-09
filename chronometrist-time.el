;;; chronometrist-time.el --- Time and date functions for Chronometrist

(require 'dash)

;;; Commentary:
;;

;;; Code:

(defun chronometrist-date (&optional date)
  "Return DATE or today in the form (YEAR MONTH DAY).

DATE can be
calendrical information returned by `decode-time'
\(YEAR MONTH DAY), in which case the same is returned, or
nil, in which case today's date is returned."
  (case (length date)
    (3 date)
    (t (cl-destructuring-bind (_ _ _ day month year _ _ _)
           (if date date (decode-time))
         (list year month day)))))

(defun chronometrist-time (&optional time))

(defun chronometrist-day-of-week->number (day-of-week)
  "Return an integer (0-6) representing DAY-OF-WEEK.

DAY-OF-WEEK should be a string, e.g. \"Sunday\" - see
`chronometrist-report-weekday-number-alist'."
  (cdr
   (assoc-string day-of-week chronometrist-report-weekday-number-alist)))

(defun chronometrist-number->day-of-week (number)
  "Return the day of the week (as a string), corresponding to NUMBER.

NUMBER should be an integer (0-6) - see
`chronometrist-report-weekday-number-alist'."
  (car
   (rassoc number chronometrist-report-weekday-number-alist)))

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:

(provide 'chronometrist-time)

;;; chronometrist-time.el ends here
