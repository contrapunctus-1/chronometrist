;;; chronometrist-queries.el --- Functions which query Chronometrist data -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabber.fr>

;; This is free and unencumbered software released into the public domain.
;;
;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.
;;
;; For more information, please refer to <https://unlicense.org>

;;; Commentary:
;;

;;; Code:

(require 'dash)
(require 'chronometrist-common)
(require 'chronometrist-events)

(defun chronometrist-last ()
  "Return the last entry from `chronometrist-file' as a plist."
  (chronometrist-sexp-last))

(cl-defun chronometrist-task-time-one-day (task &optional (date-string (chronometrist-date)))
  "Return total time spent on TASK today or (if supplied) on DATE-STRING.
The data is obtained from `chronometrist-file', via `chronometrist-events'.

DATE-STRING must be in the form \"YYYY-MM-DD\".

The return value is seconds, as an integer."
  ;; the most recent event is the last element
  (let* ((task-events    (chronometrist-task-events-in-day task date-string))
         ;; we may need it in case it's an ongoing task
         (last-event     (cl-copy-list (car (last task-events))))
         (other-events-reversed (-> task-events (reverse) (cdr))))
    (if task-events
        (->> (if (plist-member last-event :stop)
                 task-events
               ;; last-event is a currently ongoing task
               (-> (plist-put last-event :stop (chronometrist-format-time-iso8601))
                   (list)
                   (append other-events-reversed)
                   (reverse)))
             (chronometrist-events->time-list)
             (chronometrist-time-list->sum-of-intervals)
             (cadr))
      ;; no events for this task on DATE-STRING i.e. no time spent
      0)))

(defun chronometrist-active-time-one-day (&optional date-string)
  "Return the total active time on DATE (if non-nil) or today.
DATE-STRING must be in the form \"YYYY-MM-DD\".

Return value is a vector in the form [HOURS MINUTES SECONDS]."
  (->> chronometrist-task-list
       (--map (chronometrist-task-time-one-day it date-string))
       (-reduce #'+)))

(cl-defun chronometrist-statistics-count-active-days (task &optional (table chronometrist-events))
  "Return the number of days the user spent any time on TASK.
TABLE must be a hash table - if not supplied, `chronometrist-events' is used.

This will not return correct results if TABLE contains records
which span midnights. (see `chronometrist-events-clean')"
  (let ((count 0))
    (maphash (lambda (_date events)
               (when (seq-find (lambda (event)
                                 (equal (plist-get event :name) task))
                               events)
                 (cl-incf count)))
             table)
    count))

(defun chronometrist-task-events-in-day (task ts)
  "Get events for TASK on TS.
TS should be a ts struct (see `ts.el').

Returns a list of events, where each event is a property list in
the form (:name \"NAME\" :start START :stop STOP ...), where
START and STOP are ISO-8601 time strings.

This will not return correct results if TABLE contains records
which span midnights. (see `chronometrist-events-clean')"
  (->> (gethash (ts-format "%F" ts) chronometrist-events)
       (mapcar (lambda (event)
                 (when (equal task (plist-get event :name))
                   event)))
       (seq-filter #'identity)))


(provide 'chronometrist-queries)

;;; chronometrist-queries.el ends here
