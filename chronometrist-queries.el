;;; chronometrist-queries.el --- Functions which query Chronometrist data -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabber.fr>

;;; Commentary:
;;

;;; Code:

(defun chronometrist-task-time-one-day (task &optional date-string)
  "Return total time spent on TASK today or (if supplied) on DATE-STRING.
The data is obtained from `chronometrist-file', via `chronometrist-events'.

DATE-STRING must be in the form \"YYYY-MM-DD\".

The return value is a vector in the form [HOURS MINUTES SECONDS]"
  (let* ((date-string    (if date-string date-string (chronometrist-date)))
         (task-events    (chronometrist-task-events-in-day task date-string))
         (last-event     (cl-copy-list (car (last task-events))))
         (reversed-events-tail (-> task-events
                                   (reverse)
                                   (cdr))))
    (if task-events
        (->> (if (plist-member last-event :stop)
                 task-events
               ;; last-event is a currently ongoing task
               (-> (plist-put last-event :stop (chronometrist-format-time-iso8601))
                   (list)
                   (append reversed-events-tail)
                   (reverse)))
             (chronometrist-events->time-list)
             (chronometrist-time-list->sum-of-intervals)
             (cadr)
             (chronometrist-seconds-to-hms))
      ;; no events for this task on DATE-STRING i.e. no time spent
      [0 0 0])))

(defun chronometrist-active-time-one-day (&optional date-string)
  "Return the total active time on DATE (if non-nil) or today.
DATE-STRING must be in the form \"YYYY-MM-DD\".

Return value is a vector in the form [HOURS MINUTES SECONDS]."
  (->> chronometrist-task-list
       (--map (chronometrist-task-time-one-day it date-string))
       (-reduce #'chronometrist-time-add)))

(defun chronometrist-statistics-count-active-days (task &optional table)
  "Return the number of days the user spent any time on TASK.
TABLE must be a hash table - if not supplied, `chronometrist-events' is used.

This will not return correct results if TABLE contains records
which span midnights. (see `chronometrist-events-clean')"
  (let ((count 0)
        (table (if table table chronometrist-events)))
    (maphash (lambda (_date events)
               (when (seq-find (lambda (event)
                                 (equal (plist-get event :name) task))
                               events)
                 (cl-incf count)))
             table)
    count))

(defun chronometrist-task-events-in-day (task date-string)
  "Get events for TASK on DATE-STRING.
DATE-STRING must be in the form \"YYYY-MM-DD\".

Returns a list of events, where each event is a property list in
the form (:name \"NAME\" :start START :stop STOP ...), where
START and STOP are ISO-8601 time strings.

This will not return correct results if TABLE contains records
which span midnights. (see `chronometrist-events-clean')"
  (->> (gethash date-string chronometrist-events)
       (mapcar (lambda (event)
                 (when (equal task (plist-get event :name))
                   event)))
       (seq-filter #'identity)))


(provide 'chronometrist-queries)

;;; chronometrist-queries.el ends here
