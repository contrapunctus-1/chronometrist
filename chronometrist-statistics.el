;;; chronometrist-statistics.el --- View statistics for Chronometrist data -*- lexical-binding: t; -*-

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

(require 'parse-time)
(require 'cl-lib)
(require 'filenotify)

(require 'chronometrist-common)
(require 'chronometrist-time)
(require 'chronometrist-timer)
(require 'chronometrist-events)
(require 'chronometrist-statistics-custom)
(require 'chronometrist-migrate)
(require 'chronometrist-queries)

(declare-function chronometrist-refresh-file "chronometrist.el")

;; details!
;; for each activity, spent most time on doing X (where X is a
;; comment, assuming you use comments to detail what you did)

;; Really might need emacs-async for this...buttloads of big
;; calculations which will only get bigger as the timelog file grows,
;; and the more the activities, the more the calculations! I'm
;; visualizing the table loading gradually, field by field, like an
;; image in a browser.

;; TODO -
;; 1. [x] show dash instead of zero
;; 2. [x] percent for active days
;; 3. buttons
;; 4. [x] display date ranges in a nicer way
;; 5. month and year ranges
;; 6. totals for each column
;; 7. (maybe) jump between chronometrist-report and chronometrist-statistics for viewing the same week's data
;;    - in chronometrist-statistics, this only makes sense in week mode
;; 8. a 'counter' - if I have ten weeks of data and I'm on the latest,
;;    show 10/10; update this as we scroll
;;    - don't scroll past the end, as currently happens
;;    - also applicable to chronometrist-report

;; TODO - convert all functions which take dates as arguments to use
;; the (YEAR MONTH DAY) format

;;; Code:

;; ## VARIABLES ##

(defvar chronometrist-statistics--ui-state nil
  "Stores the display state for `chronometrist-statistics'.

This must be a plist in the form (:MODE :START :END).

:MODE is either 'week, 'month, 'year, 'full, or 'custom.

'week, 'month, and 'year mean display statistics
weekly/monthly/yearly respectively.

'full means display statistics from the beginning to the end of
the `chronometrist-file'.

'custom means display statistics from an arbitrary date range.

:START and :END are the start and end of the date range to be
displayed. They must be ts structs (see `ts.el').")

(defvar chronometrist-statistics--point nil)

(defvar chronometrist-statistics-mode-map)

;; ## FUNCTIONS ##

(cl-defun chronometrist-statistics-count-average-time-spent (task &optional (table chronometrist-events))
  "Return the average time the user has spent on TASK from TABLE.

TABLE must be a hash table - if not supplied,
`chronometrist-events' is used.

This will not return correct results if TABLE contains records
which span midnights. (see `chronometrist-events-clean')"
  (let ((days  0)
        (per-day-time-list))
    (maphash (lambda (key _value)
               (let ((events-in-day (chronometrist-task-events-in-day task (chronometrist-iso-date->ts key))))
                 (when events-in-day
                   (setq days (1+ days))
                   (->> events-in-day
                        (chronometrist-events->time-list)
                        (chronometrist-time-list->sum-of-intervals)
                        (list)
                        (append per-day-time-list)
                        (setq per-day-time-list)))))
             table)
    (if per-day-time-list
        (--> per-day-time-list
             (-reduce #'time-add it)
             (cadr it)
             (/ it days))
      0)))

(defun chronometrist-statistics-entries-internal (table)
  "Helper function for `chronometrist-statistics-entries'.

It simply operates on the entire hash table TABLE (see
`chronometrist-events' for table format), so ensure that TABLE is
reduced to the desired range using
`chronometrist-events-subset'."
  (mapcar (lambda (task)
            (let* ((active-days    (chronometrist-statistics-count-active-days task table))
                   (active-percent (cl-case (plist-get chronometrist-statistics--ui-state :mode)
                                     ('week (* 100 (/ active-days 7.0)))))
                   (active-percent (if (zerop active-days)
                                       (format "    % 6s" "-")
                                     (format "    %05.2f%%" active-percent)))
                   (active-days    (format "% 5s"
                                           (if (zerop active-days)
                                               "-"
                                             active-days)))
                   (average-time   (->> (chronometrist-statistics-count-average-time-spent task table)
                                        (chronometrist-seconds-to-hms)
                                        (chronometrist-format-time)
                                        (format "% 5s")))
                   (content        (vector task
                                           active-days
                                           active-percent
                                           average-time)))
              (list task content)))
          chronometrist-task-list))

(defun chronometrist-statistics-entries ()
  "Create entries to be displayed in the buffer created by `chronometrist-statistics'."
  ;; We assume that all fields in `chronometrist-statistics--ui-state' are set, so they must
  ;; be changed by the view-changing functions.
  (cl-case (plist-get chronometrist-statistics--ui-state :mode)
    ('week
     (let* ((start (plist-get chronometrist-statistics--ui-state :start))
            (end   (plist-get chronometrist-statistics--ui-state :end))
            (table (chronometrist-events-subset start end)))
       (chronometrist-statistics-entries-internal table)))
    (t ;; `chronometrist-statistics--ui-state' is nil, show current week's data
     (let* ((start      (chronometrist-previous-week-start (chronometrist-date)))
            (end        (ts-adjust 'day 7 start))
            (table      (chronometrist-events-subset start end)))
       (setq chronometrist-statistics--ui-state `(:mode week :start ,start :end ,end))
       (chronometrist-statistics-entries-internal table)))))

(defun chronometrist-statistics-print-keybind (command &optional description firstonly)
  "Insert the keybindings for COMMAND.
If DESCRIPTION is non-nil, insert that too.
If FIRSTONLY is non-nil, return only the first keybinding found."
  (insert "\n    "
          (chronometrist-format-keybinds command
                             chronometrist-statistics-mode-map
                             firstonly)
          " - "
          (if description description "")))

(defun chronometrist-statistics-format-date (date)
  "Return DATE (YEAR MONTH DAY) as a string in the form \"YYYY-MM-DD\"."
  (-let [(year month day) date]
    (format "%04d-%02d-%02d" year month day)))

(defun chronometrist-statistics-print-non-tabular ()
  "Print the non-tabular part of the buffer in `chronometrist-statistics'."
  (let ((w "\n    ")
        (inhibit-read-only t))
    (goto-char (point-max))
    (insert w)
    (insert-text-button (cl-case (plist-get chronometrist-statistics--ui-state :mode)
                          ('week "Weekly view"))
                        ;; 'action #'chronometrist-report-previous-week ;; TODO - make interactive function to accept new mode from user
                        'follow-link t)
    (insert ", from")
    (insert
     (format " %s to %s\n"
             (ts-format "%F" (plist-get chronometrist-statistics--ui-state :start))
             (ts-format "%F" (plist-get chronometrist-statistics--ui-state :end))))))

(defun chronometrist-statistics-refresh (&optional _ignore-auto _noconfirm)
  "Refresh the `chronometrist-statistics' buffer.
This does not re-read `chronometrist-file'.

The optional arguments _IGNORE-AUTO and _NOCONFIRM are ignored,
and are present solely for the sake of using this function as a
value of `revert-buffer-function'."
  (let* ((w (get-buffer-window chronometrist-statistics-buffer-name t))
         (p (point)))
    (with-current-buffer chronometrist-statistics-buffer-name
      (tabulated-list-print t nil)
      (chronometrist-statistics-print-non-tabular)
      (chronometrist-maybe-start-timer)
      (set-window-point w p))))

;; ## MAJOR MODE ##

(defvar chronometrist-statistics-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "l") #'chronometrist-open-log)
    (define-key map (kbd "b") #'chronometrist-statistics-previous-range)
    (define-key map (kbd "f") #'chronometrist-statistics-next-range)
    map)
  "Keymap used by `chronometrist-statistics-mode'.")

(define-derived-mode chronometrist-statistics-mode tabulated-list-mode "Chronometrist-Statistics"
  "Major mode for `chronometrist-statistics'."
  (make-local-variable 'tabulated-list-format)
  (setq tabulated-list-format
        [("Task"      25 t)
         ("Active days"  12 t)
         ("%% of days active"   17 t)
         ("Average time" 12 t)
         ;; ("Current streak"           10 t)
         ;; ("Last streak"              10 t)
         ;; ("Longest streak"           10 t)
         ])
  (make-local-variable 'tabulated-list-entries)
  (setq tabulated-list-entries 'chronometrist-statistics-entries)
  (make-local-variable 'tabulated-list-sort-key)
  (setq tabulated-list-sort-key '("Task" . nil))
  (tabulated-list-init-header)
  ;; (chronometrist-maybe-start-timer)
  (setq revert-buffer-function #'chronometrist-statistics-refresh)
  (unless chronometrist--fs-watch
    (setq chronometrist--fs-watch
          (file-notify-add-watch chronometrist-file
                                 '(change)
                                 #'chronometrist-refresh-file))))

;; ## COMMANDS ##

;;;###autoload
(defun chronometrist-statistics (&optional preserve-state)
  "Display statistics for data in `chronometrist-file'.

This is the 'listing command' for `chronometrist-statistics-mode'.

If a buffer called `chronometrist-statistics-buffer-name' already
exists and is visible, kill the buffer.

If PRESERVE-STATE is nil (the default when not supplied), display
data from the current week. Otherwise, display data from the week
specified by `chronometrist-statistics--ui-state'."
  (interactive)
  (chronometrist-migrate-check)
  (let* ((buffer     (get-buffer-create chronometrist-statistics-buffer-name))
         (today      (chronometrist-date))
         (week-start (chronometrist-previous-week-start today))
         (week-end   (ts-adjust 'day 6 week-start)))
    (with-current-buffer buffer
      (cond ((get-buffer-window chronometrist-statistics-buffer-name)
             (kill-buffer buffer))
            (t ;; (delete-other-windows)
             (unless preserve-state
               (setq chronometrist-statistics--ui-state `(:mode week
                                        :start ,week-start
                                        :end   ,week-end)))
             (chronometrist-common-create-file)
             (chronometrist-statistics-mode)
             (switch-to-buffer buffer)
             (chronometrist-statistics-refresh))))))

(defun chronometrist-statistics-previous-range (arg)
  "View the statistics in the previous time range.

If ARG is a numeric argument, go back that many times."
  (interactive "P")
  (let* ((arg   (if (and arg (numberp arg))
                    (abs arg)
                  1))
         (start (plist-get chronometrist-statistics--ui-state :start)))
    (cl-case (plist-get chronometrist-statistics--ui-state :mode)
      ('week
       (let* ((new-start (ts-adjust 'day -7 start))
              (new-end   (ts-adjust 'day +6 new-start)))
         (plist-put chronometrist-statistics--ui-state :start new-start)
         (plist-put chronometrist-statistics--ui-state :end   new-end))))
    (setq chronometrist-statistics--point (point))
    (kill-buffer)
    (chronometrist-statistics t)))

(defun chronometrist-statistics-next-range (arg)
  "View the statistics in the next time range.

If ARG is a numeric argument, go forward that many times."
  (interactive "P")
  (let* ((arg   (if (and arg (numberp arg))
                    (abs arg)
                  1))
         (start (plist-get chronometrist-statistics--ui-state :start)))
    (cl-case (plist-get chronometrist-statistics--ui-state :mode)
      ('week
       (let* ((new-start (ts-adjust 'day 7 start))
              (new-end   (ts-adjust 'day 6 new-start)))
         (plist-put chronometrist-statistics--ui-state :start new-start)
         (plist-put chronometrist-statistics--ui-state :end   new-end))))
    (setq chronometrist-statistics--point (point))
    (kill-buffer)
    (chronometrist-statistics t)))

(provide 'chronometrist-statistics)

;; Local Variables:
;; nameless-current-name: "chronometrist-statistics"
;; End:

;;; chronometrist-statistics.el ends here
