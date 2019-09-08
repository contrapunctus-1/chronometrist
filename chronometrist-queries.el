;;; chronometrist-queries.el --- Functions which query Chronometrist data

;;; Commentary:
;;

(defun chronometrist-task-time-one-day (task &optional date)
  "Return total time spent on PROJECT today or (if supplied) on DATE.

The data is obtained from `chronometrist-file', via `chronometrist-events'.

DATE must be in the form \"YYYY-MM-DD\".

The return value is a vector in the form [HOURS MINUTES SECONDS]"
  (let* ((date           (if date date (chronometrist-date)))
         (task-events    (chronometrist-task-events-in-day task date))
         (last-event     (copy-list (car (last task-events))))
         (reversed-events-tail (-> task-events
                                   (reverse)
                                   (cdr))))
    (if task-events
        (->> (if (plist-member last-event :stop)
                 task-events
               ;; when task is active
               (-> (plist-put last-event :stop (chronometrist-format-time-iso8601))
                   (list)
                   (append reversed-events-tail)
                   (reverse)))
             (chronometrist-events->time-list)
             (chronometrist-time-list->sum-of-intervals)
             (cadr)
             (chronometrist-seconds-to-hms))
      [0 0 0])))

(defun chronometrist-total-time-one-day (&optional date)
  "Return the total active time on DATE (if non-nil) or today.

Return value is a vector in the form [HOURS MINUTES SECONDS].

DATE must be calendrical information calendrical
information (see (info \"(elisp)Time Conversion\"))."
  (->> chronometrist--task-list
       (--map (chronometrist-task-time-one-day it date))
       (-reduce #'chronometrist-time-add)))

(provide 'chronometrist-queries)

;;; chronometrist-queries.el ends here
