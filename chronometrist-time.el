;;; chronometrist-time.el --- Time and date functions for Chronometrist -*- lexical-binding: t; -*-

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

(defun chronometrist-current-time-iso8601 (&optional unix-time)
  "Return current moment as an ISO-8601 format time string.

Optional argument UNIX-TIME should be a time value (see
`current-time') accepted by `format-time-string'."
  (format-time-string "%FT%T%z" unix-time))

;; Note - this assumes that an event never crosses >1 day. This seems
;; sufficient for all conceivable cases.
(defun chronometrist-events-midnight-spanning-p (start-time stop-time)
  "Return non-nil if START-TIME and STOP-TIME cross a midnight.

Return value is a list in the form
\((:start START-TIME
  :stop <day-start time on initial day>)
 (:start <day start time on second day>
  :stop STOP-TIME))"
  ;; FIXME - time zones are ignored; may cause issues with
  ;; time-zone-spanning events
  (let* ((first-day-start (chronometrist-day-start start-time))
         (next-day-start  (time-add first-day-start
                                    '(0 . 86400)))
         (stop-time-unix  (parse-iso8601-time-string stop-time)))
    (when (time-less-p next-day-start stop-time-unix)
      (list `(:start ,start-time
              :stop  ,(chronometrist-current-time-iso8601 first-day-start))
            `(:start ,(chronometrist-current-time-iso8601 next-day-start)
              :stop  ,stop-time)))))

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:

(provide 'chronometrist-time)

;;; chronometrist-time.el ends here
