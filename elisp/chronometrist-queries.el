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

(cl-defun chronometrist-task-time-one-day (task &optional (ts (chronometrist-date)))
  "Return total time spent on TASK today or (if supplied) on timestamp TS.
The data is obtained from `chronometrist-file', via `chronometrist-events'.

TS should be a ts struct (see `ts.el').

The return value is seconds, as an integer."
  (let* ((today-start  ts)
         (today-end    (ts-adjust 'day 1 ts))
         (events-today (chronometrist-sexp-read today-start today-end))
         (task-events  (when events-today
                         (chronometrist-filter-events-task events-today task))))
    (if task-events
        (--> (cl-loop with new with list
                      for event in task-events
                      do (setq new (chronometrist-events-maybe-split event))
                      if new append new into list
                      else collect event into list
                      finally return list)
             (chronometrist-filter-events-time it today-start today-end)
             (chronometrist-events->ts-pairs it)
             (chronometrist-ts-pairs->durations it)
             (-reduce #'+ it)
             (truncate it))
      ;; no events for this task on TS, i.e. no time spent
      0)))

(cl-defun chronometrist-active-time-one-day (&optional (ts (chronometrist-date)))
  "Return the total active time on TS (if non-nil) or today.
TS must be a ts struct (see `ts.el')."
  (->> (--map (chronometrist-task-time-one-day it ts) chronometrist-task-list)
       (-reduce #'+)
       (truncate)))

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

(defun chronometrist-filter-events-task (events task)
  "Return events for TASK from EVENTS.
EVENTS should be a list of property lists in the form (:name
\"NAME\" :start START :stop STOP ...), where START and STOP are
ISO-8601 time strings."
  (cl-loop for event in events
           when (equal task (plist-get event :name))
           collect event))

(defun chronometrist-filter-events-time (events begin end)
  "From EVENTS, return the ones between BEGIN and END."
  (cl-loop with start with stop
           for event in events
           do (setq start (chronometrist-iso-timestamp->ts (plist-get event :start))
                    stop  (chronometrist-iso-timestamp->ts (plist-get event :stop)))
           when (or (ts<= begin start)
                    (ts<= end stop))
           collect event))

(provide 'chronometrist-queries)

;;; chronometrist-queries.el ends here
