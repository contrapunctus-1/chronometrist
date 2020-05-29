;;; chronometrist-time.el --- Time and date functions for Chronometrist -*- lexical-binding: t; -*-

;; Author: contrapunctus <xmpp:contrapunctus@jabber.fr>

(require 'parse-time)
(require 'dash)
(require 's)
(require 'chronometrist-report-custom)

(declare-function chronometrist-day-start "chronometrist-events.el")

;; This is free and unencumbered software released into the public domain.
;;
;; Anyone is free to copy, modify, publish, use, compile, sell, or
;; distribute this software, either in source code form or as a compiled
;; binary, for any purpose, commercial or non-commercial, and by any
;; means.
;;
;; For more information, please refer to <https://unlicense.org>

;;; Commentary:
;; Pretty sure quite a few of these are redundant. Hopefully putting
;; them together in the same file will make it easier to figure out
;; which ones those are.

;;; Code:

(defun chronometrist-ts->iso (ts)
  (ts-format "%FT%T%z" ts))

(defun chronometrist-iso-timestamp->ts (timestamp)
  "Return new ts struct, parsing TIMESTAMP with `parse-iso8601-time-string'."
  (-let [(second minute hour day month year dow _dst utcoff)
         (decode-time
          (parse-iso8601-time-string timestamp))]
    (ts-update
     (make-ts :hour hour :minute minute :second second
              :day day   :month month   :year year
              :dow dow   :tz-offset utcoff))))

(defun chronometrist-iso-date->ts (date)
  "Return a ts struct (see `ts.el') representing DATE.
DATE should be an ISO-8601 date string (\"YYYY-MM-DD\")."
  (let* ((date-list (mapcar #'string-to-number
                            (split-string date "-")))
         (day       (caddr date-list))
         (month     (cadr date-list))
         (year      (car date-list)))
    (ts-update
     (make-ts :hour 0 :minute 0 :second 0
              :day day :month month :year year))))

(cl-defun chronometrist-date (&optional (ts (ts-now)))
  "Return a ts struct representing the time 00:00:00 on today's date.
If TS is supplied, use that date instead of today.
TS should be a ts struct (see `ts.el')."
  (ts-apply :hour 0 :minute 0 :second 0 ts))

(defun chronometrist-format-time-iso8601 (&optional unix-time)
  "Return current moment as an ISO-8601 format time string.

Optional argument UNIX-TIME should be a time value (see
`current-time') accepted by `format-time-string'."
  (format-time-string "%FT%T%z" unix-time))

(defun chronometrist-day-start (ts)
  "Return the timestamp for `chronometrist-day-start-time' on the date represented by TS.
TS must be a ts struct (see `ts.el').

Return value is a TS struct."
  (-let [(h m s) (mapcar #'string-to-number (split-string chronometrist-day-start-time ":"))]
    (ts-apply :hour h :minute m :second s ts)))

(defun chronometrist-seconds-to-hms (seconds)
  "Convert SECONDS to a vector in the form [HOURS MINUTES SECONDS].
SECONDS must be a positive integer."
  (let* ((seconds (truncate seconds))
         (s       (% seconds 60))
         (m       (% (/ seconds 60) 60))
         (h       (/ seconds 3600)))
    (list h m s)))

(defun chronometrist-interval (event)
  "Return the period of time covered by EVENT as a time value.
EVENT should be a plist (see `chronometrist-file')."
  (let ((start (plist-get event :start))
        (stop  (plist-get event :stop)))
    (time-subtract (parse-iso8601-time-string stop)
                   (parse-iso8601-time-string start))))

(provide 'chronometrist-time)

;; Local Variables:
;; nameless-current-name: "chronometrist"
;; End:

;;; chronometrist-time.el ends here
